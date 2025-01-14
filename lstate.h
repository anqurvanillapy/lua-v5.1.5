/* Global State. */

#pragma once

#include "lua.h"

#include "lobject.h"
#include "ltm.h"
#include "lzio.h"

struct lua_longjmp; /* defined in ldo.c */

/* table of globals */
#define gt(L) (&L->l_gt)

/* registry */
#define registry(L) (&G(L)->l_registry)

/* extra stack space to handle TM calls and some other extras */
#define EXTRA_STACK 5

#define BASIC_CI_SIZE 8

#define BASIC_STACK_SIZE (2 * LUA_MIN_STACK)

typedef struct stringtable {
  GCObject **hash;
  lu_int32 nuse; /* number of elements */
  int size;
} stringtable;

/*
** informations about a call
*/
typedef struct CallInfo {
  StackIndex base; /* base for this function */
  StackIndex func; /* function index in the stack */
  StackIndex top;  /* top for this function */
  const Instruction *savedpc;
  int nresults;  /* expected number of results from this function */
  int tailcalls; /* number of tail calls lost under this entry */
} CallInfo;

#define curr_func(L) (CLOSURE_VALUE(L->ci->func))
#define ci_func(ci) (CLOSURE_VALUE((ci)->func))
#define f_isLua(ci) (!ci_func(ci)->c.isC)
#define isLua(ci) (IS_TYPE_FUNCTION((ci)->func) && f_isLua(ci))

/*
** `global state', shared by all threads of this state
*/
typedef struct global_State {
  stringtable strt;   /* hash table for strings */
  lua_Alloc frealloc; /* function to reallocate memory */
  void *ud;           /* auxiliary data to `frealloc' */
  lu_byte currentwhite;
  lu_byte gcstate;     /* state of garbage collector */
  int sweepstrgc;      /* position of sweep in `strt' */
  GCObject *rootgc;    /* list of all collectable objects */
  GCObject **sweepgc;  /* position of sweep in `rootgc' */
  GCObject *gray;      /* list of gray objects */
  GCObject *grayagain; /* list of objects to be traversed atomically */
  GCObject *weak;      /* list of weak tables (to be cleared) */
  GCObject *tmudata;   /* last element of list of userdata to be GC */
  Mbuffer buff;        /* temporary buffer for string concatentation */
  lu_mem GCthreshold;
  lu_mem totalbytes;   /* number of bytes currently allocated */
  lu_mem estimate;     /* an estimate of number of bytes actually in use */
  lu_mem gcdept;       /* how much GC is `behind schedule' */
  int gcpause;         /* size of pause between successive GCs */
  int gcstepmul;       /* GC `granularity' */
  lua_CFunction panic; /* to be called in unprotected errors */
  TaggedValue l_registry;
  struct lua_State *mainthread;
  UpVal uvhead; /* head of double-linked list of all open upvalues */
  struct Table *mt[NUM_TAGS]; /* metatables for basic types */
  TString *tmname[TM_N];      /* array with tag-method names */
} global_State;

// Per-thread state.
struct lua_State {
  GCHeaderFields;

  lu_byte status;

  // First free slot in the stack.
  StackIndex top;
  // Base of current function.
  StackIndex base;

  global_State *l_G;

  // Call info for current function.
  CallInfo *ci;
  // Saved PC of current function.
  const Instruction *savedPC;
  // Last free slot in the stack.
  StackIndex stackLast;
  // Stack base.
  StackIndex stack;
  // Points after end of ci array.
  CallInfo *endCI;
  // Points to the CallInfo array.
  CallInfo *baseCI;
  int stackSize;
  // Size of baseCI.
  int ciSize;

  // Number of nested C calls.
  unsigned short nestedCCallNum;
  // Nested C calls when resuming coroutine.
  unsigned short nestedCCallBaseNum;

  lu_byte hookMask;
  lu_byte allowHook;
  int baseHookCount;
  int hookCount;
  lua_Hook hook;

  // Table of globals.
  TaggedValue l_gt;
  // Temporary place for environments.
  TaggedValue env;

  // List of open upvalues in this stack.
  GCObject *openUpval;
  GCObject *gcList;

  // Current error recover point.
  struct lua_longjmp *errorJmp;
  // current error handling function (stack index).
  ptrdiff_t errFunc;
};

#define G(L) (L->l_G)

/*
** Union of all collectable objects
*/
union GCObject {
  GCHeader gch;
  union TString ts;
  union Udata u;
  union Closure cl;
  struct Table h;
  struct Prototype p;
  struct UpVal uv;
  struct lua_State th; /* thread */
};

/* macros to convert a GCObject into a specific value */
#define rawgco2ts(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_STRING, &((o)->ts))
#define gco2ts(o) (&rawgco2ts(o)->tsv)
#define rawgco2u(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_USERDATA, &((o)->u))
#define gco2u(o) (&rawgco2u(o)->uv)
#define gco2cl(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_FUNCTION, &((o)->cl))
#define gco2h(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_TABLE, &((o)->h))
#define gco2p(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_PROTO, &((o)->p))
#define gco2uv(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_UPVALUE, &((o)->uv))
#define ngcotouv(o)                                                            \
  CHECK_EXPR((o) == NULL || (o)->gch.tt == LUA_TYPE_UPVALUE, &((o)->uv))
#define gco2th(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_THREAD, &((o)->th))

/* macro to convert any Lua object into a GCObject */
#define LuaObjectToGCObject(v) (cast(GCObject *, (v)))

LUAI_FUNC lua_State *luaE_newthread(lua_State *L);
LUAI_FUNC void luaE_freethread(lua_State *L, lua_State *L1);
