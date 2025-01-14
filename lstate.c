/* Global State. */

#include <stddef.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"

#define state_size(x) (sizeof(x) + LUAI_EXTRASPACE)
#define fromstate(l) (cast(lu_byte *, (l)) - LUAI_EXTRASPACE)
#define tostate(l) (cast(lua_State *, cast(lu_byte *, l) + LUAI_EXTRASPACE))

/*
** Main thread combines a thread state and the global state
*/
typedef struct LG {
  lua_State l;
  global_State g;
} LG;

static void stack_init(lua_State *L1, lua_State *L) {
  /* initialize CallInfo array */
  L1->baseCI = luaM_newvector(L, BASIC_CI_SIZE, CallInfo);
  L1->ci = L1->baseCI;
  L1->ciSize = BASIC_CI_SIZE;
  L1->endCI = L1->baseCI + L1->ciSize - 1;
  /* initialize stack array */
  L1->stack = luaM_newvector(L, BASIC_STACK_SIZE + EXTRA_STACK, TaggedValue);
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
  luaM_freearray(L, L1->baseCI, L1->ciSize, CallInfo);
  luaM_freearray(L, L1->stack, L1->stackSize, TaggedValue);
}

/*
** open parts that may cause memory-allocation errors
*/
static void f_luaopen(lua_State *L, void *) {
  global_State *g = G(L);
  stack_init(L, L);                             /* init stack */
  SET_TABLE(L, gt(L), luaH_new(L, 0, 2));       /* table of globals */
  SET_TABLE(L, registry(L), luaH_new(L, 0, 2)); /* registry */
  luaS_resize(L, MINSTRTABSIZE); /* initial size of string table */
  luaT_init(L);
  luaX_init(L);
  luaS_fix(luaS_newliteral(L, MEMERRMSG));
  g->GCthreshold = 4 * g->totalbytes;
}

static void preinit_state(lua_State *L, global_State *g) {
  G(L) = g;
  L->stack = NULL;
  L->stackSize = 0;
  L->errorJmp = NULL;
  L->hook = NULL;
  L->hookMask = 0;
  L->baseHookCount = 0;
  L->allowHook = 1;
  resethookcount(L);
  L->openUpval = NULL;
  L->ciSize = 0;
  L->nestedCCallNum = L->nestedCCallBaseNum = 0;
  L->status = 0;
  L->baseCI = L->ci = NULL;
  L->savedPC = NULL;
  L->errFunc = 0;
  SET_NIL(gt(L));
}

static void close_state(lua_State *L) {
  global_State *g = G(L);
  luaF_close(L, L->stack); /* close all upvalues for this thread */
  luaC_freeall(L);         /* collect all objects */
  lua_assert(g->rootgc == LuaObjectToGCObject(L));
  lua_assert(g->strt.nuse == 0);
  luaM_freearray(L, G(L)->strt.hash, G(L)->strt.size, TString *);
  luaZ_freebuffer(L, &g->buff);
  freestack(L, L);
  lua_assert(g->totalbytes == sizeof(LG));
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
  lua_assert(iswhite(LuaObjectToGCObject(L1)));
  return L1;
}

void luaE_freethread(lua_State *L, lua_State *L1) {
  luaF_close(L1, L1->stack); /* close all upvalues for this thread */
  lua_assert(L1->openUpval == NULL);
  luai_userstatefree(L1);
  freestack(L, L1);
  luaM_freemem(L, fromstate(L1), state_size(lua_State));
}

LUA_API lua_State *lua_newstate(lua_Alloc f, void *ud) {
  int i;
  lua_State *L;
  global_State *g;
  void *l = (*f)(ud, NULL, 0, state_size(LG));
  if (l == NULL) {
    return NULL;
  }
  L = tostate(l);
  g = &((LG *)L)->g;
  L->next = NULL;
  L->tt = LUA_TYPE_THREAD;
  g->currentwhite = bit2mask(WHITE0BIT, FIXEDBIT);
  L->marked = luaC_white(g);
  set2bits(L->marked, FIXEDBIT, SFIXEDBIT);
  preinit_state(L, g);
  g->frealloc = f;
  g->ud = ud;
  g->mainthread = L;
  g->uvhead.u.l.prev = &g->uvhead;
  g->uvhead.u.l.next = &g->uvhead;
  g->GCthreshold = 0; /* mark it as unfinished state */
  g->strt.size = 0;
  g->strt.nuse = 0;
  g->strt.hash = NULL;
  SET_NIL(registry(L));
  luaZ_initbuffer(L, &g->buff);
  g->panic = NULL;
  g->gcstate = GCSpause;
  g->rootgc = LuaObjectToGCObject(L);
  g->sweepstrgc = 0;
  g->sweepgc = &g->rootgc;
  g->gray = NULL;
  g->grayagain = NULL;
  g->weak = NULL;
  g->tmudata = NULL;
  g->totalbytes = sizeof(LG);
  g->gcpause = LUAI_GCPAUSE;
  g->gcstepmul = LUAI_GCMUL;
  g->gcdept = 0;
  for (i = 0; i < NUM_TAGS; i++) {
    g->mt[i] = NULL;
  }
  if (luaD_rawrunprotected(L, f_luaopen, NULL) != 0) {
    /* memory allocation error: free partial state */
    close_state(L);
    L = NULL;
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
    L->nestedCCallNum = L->nestedCCallBaseNum = 0;
  } while (luaD_rawrunprotected(L, callallgcTM, NULL) != 0);
  lua_assert(G(L)->tmudata == NULL);
  luai_userstateclose(L);
  close_state(L);
}
