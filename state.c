#include <stddef.h>

#include "closure.h"
#include "debug.h"
#include "gc.h"
#include "intern.h"
#include "lexer.h"
#include "memory.h"
#include "stack.h"
#include "state.h"
#include "table.h"
#include "tag.h"

#define STATE_SIZE(x) (sizeof(x) + LUAI_EXTRASPACE)
#define FROM_STATE(l) ((uint8_t *)(l) - LUAI_EXTRASPACE)
#define TO_STATE(l) ((lua_State *)((uint8_t *)(l) + LUAI_EXTRASPACE))

typedef struct MainThread {
  lua_State l;
  GlobalState g;
} MainThread;

static void initStack(lua_State *L1, lua_State *L) {
  // Initialize CallInfo array.
  L1->baseCI = Mem_newVec(L, BASIC_CI_SIZE, CallInfo);
  L1->ci = L1->baseCI;
  L1->ciSize = BASIC_CI_SIZE;
  L1->endCI = L1->baseCI + L1->ciSize - 1;

  // Initialize stack array.
  L1->stack = Mem_newVec(L, BASIC_STACK_SIZE + EXTRA_STACK, Value);
  L1->stackSize = BASIC_STACK_SIZE + EXTRA_STACK;
  L1->top = L1->stack;
  L1->stackLast = L1->stack + (L1->stackSize - EXTRA_STACK) - 1;

  // Initialize first CallInfo,
  L1->ci->func = L1->top;
  // Function entry for this CallInfo.
  SET_NIL(L1->top++);
  L1->base = L1->ci->base = L1->top;
  L1->ci->top = L1->top + LUA_MIN_STACK;
}

static void freeStack(lua_State *L, lua_State *L1) {
  Mem_freeVec(L, L1->baseCI, L1->ciSize, CallInfo);
  Mem_freeVec(L, L1->stack, L1->stackSize, Value);
}

// Initialize the components with heap usage that might fail upon OoM.
static void initWithHeap(lua_State *L, void *) {
  GlobalState *g = G(L);
  initStack(L, L);
  SET_TABLE(L, GLOBALS(L), Table_new(L, 0, 2));
  SET_TABLE(L, REGISTRY(L), Table_new(L, 0, 2));
  StringPool_resize(L, MINSTRTABSIZE);
  luaT_init(L);
  Lexer_init(L);
  String_intern(String_createLiteral(L, MEMERRMSG));
  g->GCthreshold = 4 * g->totalbytes;
}

static void initPartialState(lua_State *L, GlobalState *g) {
  G(L) = g;
  L->stack = nullptr;
  L->stackSize = 0;
  L->errorJmp = nullptr;
  L->hook = nullptr;
  L->hookMask = 0;
  L->baseHookCount = 0;
  L->allowHook = 1;
  RESET_HOOK_COUNT(L);
  L->openUpval = nullptr;
  L->ciSize = 0;
  L->nestedCCallsNum = 0;
  L->nestedCCallsBaseNum = 0;
  L->status = LUA_RUNNING;
  L->baseCI = nullptr;
  L->ci = nullptr;
  L->savedPC = nullptr;
  L->errFunc = 0;
  SET_NIL(GLOBALS(L));
}

static void closeState(lua_State *L) {
  GlobalState *g = G(L);
  // Close all upvalues for this thread.
  luaF_close(L, L->stack);
  // Collect all objects.
  luaC_freeall(L);
  assert(g->rootgc == LuaObjectToGCObject(L));
  assert(g->pool.itemsNum == 0);
  Mem_freeVec(L, G(L)->pool.buckets, G(L)->pool.bucketsSize, String *);
  StringBuilder_free(L, &g->buff);
  freeStack(L, L);
  assert(g->totalbytes == sizeof(MainThread));
  g->alloc(g->allocData, FROM_STATE(L), STATE_SIZE(MainThread), 0);
}

lua_State *State_newThread(lua_State *L) {
  lua_State *L1 = TO_STATE(Mem_alloc(L, STATE_SIZE(lua_State)));
  luaC_link(L, LuaObjectToGCObject(L1), LUA_TYPE_THREAD);
  initPartialState(L1, G(L));
  initStack(L1, L);
  // Share the table of global variables.
  SET_OBJECT_TO_NEW(L, GLOBALS(L1), GLOBALS(L));
  L1->hookMask = L->hookMask;
  L1->baseHookCount = L->baseHookCount;
  L1->hook = L->hook;
  RESET_HOOK_COUNT(L1);
  assert(iswhite(LuaObjectToGCObject(L1)));
  return L1;
}

void State_freeThread(lua_State *L, lua_State *L1) {
  // Close all upvalues for this thread.
  luaF_close(L1, L1->stack);
  assert(L1->openUpval == nullptr);
  luai_userstatefree(L1);
  freeStack(L, L1);
  Mem_free(L, FROM_STATE(L1), STATE_SIZE(lua_State));
}

LUA_API lua_State *lua_newstate(lua_Alloc f, void *allocData) {
  void *l = f(allocData, nullptr, 0, STATE_SIZE(MainThread));
  if (l == nullptr) {
    return nullptr;
  }
  lua_State *L = TO_STATE(l);
  GlobalState *g = &((MainThread *)L)->g;
  L->header.next = nullptr;
  L->header.tt = LUA_TYPE_THREAD;
  g->currentwhite = bit2mask(WHITE0BIT, FIXEDBIT);
  L->header.marked = luaC_white(g);
  set2bits(L->header.marked, FIXEDBIT, SFIXEDBIT);
  initPartialState(L, g);
  g->alloc = f;
  g->allocData = allocData;
  g->mainthread = L;
  g->uvhead.u.l.prev = &g->uvhead;
  g->uvhead.u.l.next = &g->uvhead;
  // Mark it as unfinished state.
  g->GCthreshold = 0;
  g->pool.bucketsSize = 0;
  g->pool.itemsNum = 0;
  g->pool.buckets = nullptr;
  SET_NIL(REGISTRY(L));
  StringBuilder_init(L, &g->buff);
  g->panic = nullptr;
  g->gcstate = GCSpause;
  g->rootgc = LuaObjectToGCObject(L);
  g->sweepstrgc = 0;
  g->sweepgc = &g->rootgc;
  g->gray = nullptr;
  g->grayagain = nullptr;
  g->weak = nullptr;
  g->tmudata = nullptr;
  g->totalbytes = sizeof(MainThread);
  g->gcpause = LUAI_GCPAUSE;
  g->gcstepmul = LUAI_GCMUL;
  g->gcdept = 0;
  for (size_t i = 0; i < NUM_TYPES; i++) {
    g->mt[i] = nullptr;
  }
  if (luaD_rawrunprotected(L, initWithHeap, nullptr) != 0) {
    /* memory allocation error: free partial state */
    closeState(L);
    L = nullptr;
  } else {
    luai_userstateopen(L);
  }
  return L;
}

static void callGcTm(lua_State *L, void *) {
  luaC_callGCTM(L); /* call GC metamethods for all udata */
}

LUA_API void lua_close(lua_State *L) {
  L = G(L)->mainthread; /* only the main thread can be closed */
  lua_lock(L);
  luaF_close(L, L->stack);  /* close all upvalues for this thread */
  luaC_separateudata(L, 1); /* separate udata that have GC metamethods */
  L->errFunc = 0;           /* no error function during GC metamethods */
  do {                      /* repeat until no more errors */
    L->ci = L->baseCI;
    L->base = L->top = L->ci->base;
    L->nestedCCallsNum = L->nestedCCallsBaseNum = 0;
  } while (luaD_rawrunprotected(L, callGcTm, nullptr) != 0);
  assert(G(L)->tmudata == nullptr);
  luai_userstateclose(L);
  closeState(L);
}
