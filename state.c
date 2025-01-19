#include <stddef.h>

#include "lua.h"

#include "closure.h"
#include "debug.h"
#include "gc.h"
#include "intern.h"
#include "lexer.h"
#include "ltm.h"
#include "memory.h"
#include "stack.h"
#include "state.h"
#include "table.h"

#define state_size(x) (sizeof(x) + LUAI_EXTRASPACE)
#define fromstate(l) (cast(uint8_t *, (l)) - LUAI_EXTRASPACE)
#define tostate(l) (cast(lua_State *, cast(uint8_t *, l) + LUAI_EXTRASPACE))

/*
** Main thread combines a thread state and the global state
*/
typedef struct LG {
  lua_State l;
  GlobalState g;
} LG;

static void stack_init(lua_State *L1, lua_State *L) {
  /* initialize CallInfo array */
  L1->baseCI = luaM_newvector(L, BASIC_CI_SIZE, CallInfo);
  L1->ci = L1->baseCI;
  L1->ciSize = BASIC_CI_SIZE;
  L1->endCI = L1->baseCI + L1->ciSize - 1;
  /* initialize stack array */
  L1->stack = luaM_newvector(L, BASIC_STACK_SIZE + EXTRA_STACK, Value);
  L1->stackSize = BASIC_STACK_SIZE + EXTRA_STACK;
  L1->top = L1->stack;
  L1->stackLast = L1->stack + (L1->stackSize - EXTRA_STACK) - 1;
  /* initialize first ci */
  L1->ci->func = L1->top;
  SET_NIL(L1->top++); /* `function' entry for this `ci' */
  L1->base = L1->ci->base = L1->top;
  L1->ci->top = L1->top + LUA_MIN_STACK;
}

static void freestack(lua_State *L, lua_State *L1) {
  luaM_freeArray(L, L1->baseCI, L1->ciSize, CallInfo);
  luaM_freeArray(L, L1->stack, L1->stackSize, Value);
}

/*
** open parts that may cause memory-allocation errors
*/
static void f_luaopen(lua_State *L, void *) {
  GlobalState *g = G(L);
  stack_init(L, L);                             /* init stack */
  SET_TABLE(L, gt(L), luaH_new(L, 0, 2));       /* table of globals */
  SET_TABLE(L, registry(L), luaH_new(L, 0, 2)); /* registry */
  StringPool_resize(L, MINSTRTABSIZE); /* initial size of string table */
  luaT_init(L);
  luaX_init(L);
  String_intern(String_createLiteral(L, MEMERRMSG));
  g->GCthreshold = 4 * g->totalbytes;
}

static void preinit_state(lua_State *L, GlobalState *g) {
  G(L) = g;
  L->stack = nullptr;
  L->stackSize = 0;
  L->errorJmp = nullptr;
  L->hook = nullptr;
  L->hookMask = 0;
  L->baseHookCount = 0;
  L->allowHook = 1;
  resethookcount(L);
  L->openUpval = nullptr;
  L->ciSize = 0;
  L->nestedCCallsNum = L->nestedCCallsBaseNum = 0;
  L->status = 0;
  L->baseCI = L->ci = nullptr;
  L->savedPC = nullptr;
  L->errFunc = 0;
  SET_NIL(gt(L));
}

static void close_state(lua_State *L) {
  GlobalState *g = G(L);
  luaF_close(L, L->stack); /* close all upvalues for this thread */
  luaC_freeall(L);         /* collect all objects */
  DEBUG_ASSERT(g->rootgc == LuaObjectToGCObject(L));
  DEBUG_ASSERT(g->pool.itemsNum == 0);
  luaM_freeArray(L, G(L)->pool.buckets, G(L)->pool.bucketsSize, String *);
  luaZ_freebuffer(L, &g->buff);
  freestack(L, L);
  DEBUG_ASSERT(g->totalbytes == sizeof(LG));
  (*g->frealloc)(g->ud, fromstate(L), state_size(LG), 0);
}

lua_State *luaE_newthread(lua_State *L) {
  lua_State *L1 = tostate(luaM_malloc(L, state_size(lua_State)));
  luaC_link(L, LuaObjectToGCObject(L1), LUA_TYPE_THREAD);
  preinit_state(L1, G(L));
  stack_init(L1, L);                   /* init stack */
  SET_OBJECT_TO_NEW(L, gt(L1), gt(L)); /* share table of globals */
  L1->hookMask = L->hookMask;
  L1->baseHookCount = L->baseHookCount;
  L1->hook = L->hook;
  resethookcount(L1);
  DEBUG_ASSERT(iswhite(LuaObjectToGCObject(L1)));
  return L1;
}

void luaE_freethread(lua_State *L, lua_State *L1) {
  luaF_close(L1, L1->stack); /* close all upvalues for this thread */
  DEBUG_ASSERT(L1->openUpval == NULL);
  luai_userstatefree(L1);
  freestack(L, L1);
  luaM_freemem(L, fromstate(L1), state_size(lua_State));
}

LUA_API lua_State *lua_newstate(lua_Alloc f, void *ud) {
  int i;
  lua_State *L;
  GlobalState *g;
  void *l = (*f)(ud, NULL, 0, state_size(LG));
  if (l == NULL) {
    return nullptr;
  }
  L = tostate(l);
  g = &((LG *)L)->g;
  L->header.next = nullptr;
  L->header.tt = LUA_TYPE_THREAD;
  g->currentwhite = bit2mask(WHITE0BIT, FIXEDBIT);
  L->header.marked = luaC_white(g);
  set2bits(L->header.marked, FIXEDBIT, SFIXEDBIT);
  preinit_state(L, g);
  g->frealloc = f;
  g->ud = ud;
  g->mainthread = L;
  g->uvhead.u.l.prev = &g->uvhead;
  g->uvhead.u.l.next = &g->uvhead;
  g->GCthreshold = 0; /* mark it as unfinished state */
  g->pool.bucketsSize = 0;
  g->pool.itemsNum = 0;
  g->pool.buckets = nullptr;
  SET_NIL(registry(L));
  luaZ_initbuffer(L, &g->buff);
  g->panic = nullptr;
  g->gcstate = GCSpause;
  g->rootgc = LuaObjectToGCObject(L);
  g->sweepstrgc = 0;
  g->sweepgc = &g->rootgc;
  g->gray = nullptr;
  g->grayagain = nullptr;
  g->weak = nullptr;
  g->tmudata = nullptr;
  g->totalbytes = sizeof(LG);
  g->gcpause = LUAI_GCPAUSE;
  g->gcstepmul = LUAI_GCMUL;
  g->gcdept = 0;
  for (i = 0; i < NUM_TYPES; i++) {
    g->mt[i] = nullptr;
  }
  if (luaD_rawrunprotected(L, f_luaopen, NULL) != 0) {
    /* memory allocation error: free partial state */
    close_state(L);
    L = nullptr;
  } else {
    luai_userstateopen(L);
  }
  return L;
}

static void callallgcTM(lua_State *L, void *) {
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
  } while (luaD_rawrunprotected(L, callallgcTM, NULL) != 0);
  DEBUG_ASSERT(G(L)->tmudata == NULL);
  luai_userstateclose(L);
  close_state(L);
}
