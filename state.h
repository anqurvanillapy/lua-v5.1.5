// Global and (thread-)local states.

#pragma once

#include "buffer.h"
#include "object.h"
#include "tag.h"

#define GLOBALS(L) (&L->l_gt)
#define REGISTRY(L) (&G(L)->l_registry)

/* extra stack space to handle TM calls and some other extras */
#define EXTRA_STACK 5

#define BASIC_CI_SIZE 8

#define BASIC_STACK_SIZE (2 * LUA_MIN_STACK)

typedef struct StringPool {
  GCObject **buckets;
  size_t bucketsSize;
  size_t itemsNum;
} StringPool;

typedef struct CallInfo {
  StackIndex base; /* base for this function */
  StackIndex func; /* function index in the stack */
  StackIndex top;  /* top for this function */
  const Instruction *savedpc;
  int nresults;  /* expected number of results from this function */
  int tailcalls; /* number of tail calls lost under this entry */
} CallInfo;

#define CUR_FUNC(L) (CLOSURE_VALUE(L->ci->func))
#define ci_func(ci) (CLOSURE_VALUE((ci)->func))
#define f_isLua(ci) (!ci_func(ci)->c.header.isC)
#define isLua(ci) (IS_TYPE_FUNCTION((ci)->func) && f_isLua(ci))

// The global states shared by all threads.
typedef struct GlobalState {
  StringPool pool;

  lua_Alloc alloc;
  void *allocData;

  uint8_t currentwhite;
  uint8_t gcstate;     /* state of garbage collector */
  size_t sweepstrgc;   /* position of sweep in `pool' */
  GCObject *rootgc;    /* list of all collectable objects */
  GCObject **sweepgc;  /* position of sweep in `rootgc' */
  GCObject *gray;      /* list of gray objects */
  GCObject *grayagain; /* list of objects to be traversed atomically */
  GCObject *weak;      /* list of weak tables (to be cleared) */
  GCObject *tmudata;   /* last element of list of userdata to be GC */
  StringBuilder buff;  /* temporary buffer for string concatentation */
  size_t GCthreshold;
  size_t totalbytes;   /* number of bytes currently allocated */
  size_t estimate;     /* an estimate of number of bytes actually in use */
  size_t gcdept;       /* how much GC is `behind schedule' */
  int gcpause;         /* size of pause between successive GCs */
  int gcstepmul;       /* GC `granularity' */
  lua_CFunction panic; /* to be called in unprotected errors */
  Value l_registry;
  struct lua_State *mainthread;
  Upvalue uvhead; /* head of double-linked list of all open upvalues */
  struct Table *mt[NUM_TYPES]; /* metatables for basic types */
  String *tmname[TM_N];        /* array with tag-method names */
} GlobalState;

// Per-thread state.
struct lua_State {
  GCHeader header;

  lua_Status status;

  // First free slot in the stack.
  StackIndex top;
  // Base of current function.
  StackIndex base;

  GlobalState *l_G;

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
  unsigned short nestedCCallsNum;
  // Nested C calls when resuming coroutine.
  unsigned short nestedCCallsBaseNum;

  uint8_t hookMask;
  bool allowHook;
  int baseHookCount;
  int hookCount;
  lua_Hook hook;

  // Table of globals.
  Value l_gt;
  // Temporary place for environments.
  Value env;

  // List of open upvalues in this stack.
  GCObject *openUpval;
  GCObject *gcList;

  // Current error recover point.
  struct Ctx *errorJmp;
  // current error handling function (stack index).
  ptrdiff_t errFunc;
};

#define G(L) (L->l_G)

/*
** Union of all collectable objects
*/
union GCObject {
  GCHeader gch;
  struct String ts;
  struct Userdata u;
  union Closure cl;
  struct Table h;
  struct Prototype p;
  struct Upvalue uv;
  struct lua_State th; /* thread */
};

/* macros to convert a GCObject into a specific value */
#define gco2ts(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_STRING, &((o)->ts))
#define gco2u(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_USERDATA, &((o)->u))
#define gco2cl(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_FUNCTION, &((o)->cl))
#define gco2h(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_TABLE, &((o)->h))
#define gco2p(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_PROTO, &((o)->p))
#define gco2uv(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_UPVALUE, &((o)->uv))
#define ngcotouv(o)                                                            \
  CHECK_EXPR((o) == NULL || (o)->gch.tt == LUA_TYPE_UPVALUE, &((o)->uv))
#define gco2th(o) CHECK_EXPR((o)->gch.tt == LUA_TYPE_THREAD, &((o)->th))

/* macro to convert any Lua object into a GCObject */
#define LuaObjectToGCObject(v) (cast(GCObject *, (v)))

LUAI_FUNC lua_State *State_newThread(lua_State *L);
LUAI_FUNC void State_freeThread(lua_State *L, lua_State *L1);
