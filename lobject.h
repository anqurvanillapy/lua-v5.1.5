/* Type definitions for Lua objects. */

#pragma once

#include <stdarg.h>

#include "llimits.h"
#include "lua.h"

/* tags for values visible from Lua */
#define LAST_TAG LUA_TYPE_THREAD

#define NUM_TAGS (LAST_TAG + 1)

/*
** Extra tags for non-values
*/
#define LUA_TYPE_PROTO (LAST_TAG + 1)
#define LUA_TYPE_UPVALUE (LAST_TAG + 2)
#define LUA_TYPE_DEAD (LAST_TAG + 3) // GC-able dead object

typedef union GCObject GCObject;

#define GCHeaderFields                                                         \
  GCObject *next;                                                              \
  lu_byte tt;                                                                  \
  lu_byte marked

typedef struct GCHeader {
  GCHeaderFields;
} GCHeader;

typedef union {
  GCObject *gc;
  void *p;
  lua_Number n;
  bool b;
} Value;

#define TValueFields                                                           \
  Value value;                                                                 \
  int tt

// Tagged Values.
typedef struct TValue {
  TValueFields;
} TValue;

#define IS_TYPE_NIL(o) ((o)->tt == LUA_TYPE_NIL)
#define IS_TYPE_NUMBER(o) ((o)->tt == LUA_TYPE_NUMBER)
#define IS_TYPE_STRING(o) ((o)->tt == LUA_TYPE_STRING)
#define IS_TYPE_TABLE(o) ((o)->tt == LUA_TYPE_TABLE)
#define IS_TYPE_FUNCTION(o) ((o)->tt == LUA_TYPE_FUNCTION)
#define IS_TYPE_BOOLEAN(o) ((o)->tt == LUA_TYPE_BOOLEAN)
#define IS_TYPE_USERDATA(o) ((o)->tt == LUA_TYPE_USERDATA)
#define IS_TYPE_THREAD(o) ((o)->tt == LUA_TYPE_THREAD)
#define IS_TYPE_PTR(o) ((o)->tt == LUA_TYPE_PTR)

#define IS_FALSE(o) (IS_TYPE_NIL(o) || (IS_TYPE_BOOLEAN(o) && !BOOL_VALUE(o)))

/* Macros to access values */
#define GET_TYPE(o) ((o)->tt)
#define GC_VALUE(o) CHECK_EXPR(iscollectable(o), (o)->value.gc)
#define PTR_VALUE(o) CHECK_EXPR(IS_TYPE_PTR(o), (o)->value.p)
#define NUMBER_VALUE(o) CHECK_EXPR(IS_TYPE_NUMBER(o), (o)->value.n)
#define RAW_STRING_VALUE(o) CHECK_EXPR(IS_TYPE_STRING(o), &(o)->value.gc->ts)
#define STRING_VALUE(o) (&RAW_STRING_VALUE(o)->tsv)
#define RAW_USERDATA_VALUE(o) CHECK_EXPR(IS_TYPE_USERDATA(o), &(o)->value.gc->u)
#define USERDATA_VALUE(o) (&RAW_USERDATA_VALUE(o)->uv)
#define CLOSURE_VALUE(o) CHECK_EXPR(IS_TYPE_FUNCTION(o), &(o)->value.gc->cl)
#define TABLE_VALUE(o) CHECK_EXPR(IS_TYPE_TABLE(o), &(o)->value.gc->h)
#define BOOL_VALUE(o) CHECK_EXPR(IS_TYPE_BOOLEAN(o), (o)->value.b)
#define THREAD_VALUE(o) CHECK_EXPR(IS_TYPE_THREAD(o), &(o)->value.gc->th)

/*
** for internal debug only
*/
#define CHECK_CONSISTENCY(obj)                                                 \
  lua_assert(!iscollectable(obj) || (GET_TYPE(obj) == (obj)->value.gc->gch.tt))
#define CHECK_LIVENESS(g, obj)                                                 \
  lua_assert(!iscollectable(obj) ||                                            \
             ((GET_TYPE(obj) == (obj)->value.gc->gch.tt) &&                    \
              !isdead(g, (obj)->value.gc)))

/* Macros to set values */
#define setnilvalue(obj) ((obj)->tt = LUA_TYPE_NIL)

#define setnvalue(obj, x)                                                      \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.n = (x);                                                        \
    i_o->tt = LUA_TYPE_NUMBER;                                                 \
  } while (0)

#define setpvalue(obj, x)                                                      \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.p = (x);                                                        \
    i_o->tt = LUA_TYPE_PTR;                                                    \
  } while (0)

#define setbvalue(obj, x)                                                      \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.b = (x);                                                        \
    i_o->tt = LUA_TYPE_BOOLEAN;                                                \
  } while (0)

#define setsvalue(L, obj, x)                                                   \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_STRING;                                                 \
    CHECK_LIVENESS(G(L), i_o);                                                 \
  } while (0)

#define setuvalue(L, obj, x)                                                   \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_USERDATA;                                               \
    CHECK_LIVENESS(G(L), i_o);                                                 \
  } while (0)

#define setthvalue(L, obj, x)                                                  \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_THREAD;                                                 \
    CHECK_LIVENESS(G(L), i_o);                                                 \
  } while (0)

#define setclvalue(L, obj, x)                                                  \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_FUNCTION;                                               \
    CHECK_LIVENESS(G(L), i_o);                                                 \
  } while (0)

#define sethvalue(L, obj, x)                                                   \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_TABLE;                                                  \
    CHECK_LIVENESS(G(L), i_o);                                                 \
  } while (0)

#define setptvalue(L, obj, x)                                                  \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_PROTO;                                                  \
    CHECK_LIVENESS(G(L), i_o);                                                 \
  } while (0)

#define setobj(L, obj1, obj2)                                                  \
  do {                                                                         \
    const TValue *o2 = (obj2);                                                 \
    TValue *o1 = (obj1);                                                       \
    o1->value = o2->value;                                                     \
    o1->tt = o2->tt;                                                           \
    CHECK_LIVENESS(G(L), o1);                                                  \
  } while (0)

/*
** different types of sets, according to destination
*/

/* from stack to (same) stack */
#define setobjs2s setobj
/* to stack (not from same stack) */
#define setobj2s setobj
#define setsvalue2s setsvalue
#define sethvalue2s sethvalue
#define setptvalue2s setptvalue
/* from table to same table */
#define setobjt2t setobj
/* to table */
#define setobj2t setobj
/* to new object */
#define setobj2n setobj
#define setsvalue2n setsvalue

#define setttype(obj, tt) (GET_TYPE(obj) = (tt))

#define iscollectable(o) (GET_TYPE(o) >= LUA_TYPE_STRING)

typedef TValue *StkId; /* index to stack elements */

/*
** String headers for string table
*/
typedef union TString {
  L_Umaxalign dummy; /* ensures maximum alignment for strings */
  struct {
    GCHeaderFields;
    lu_byte reserved;
    unsigned int hash;
    size_t len;
  } tsv;
} TString;

#define getstr(ts) cast(const char *, (ts) + 1)
#define svalue(o) getstr(RAW_STRING_VALUE(o))

typedef union Udata {
  L_Umaxalign dummy; /* ensures maximum alignment for `local' udata */
  struct {
    GCHeaderFields;
    struct Table *metatable;
    struct Table *env;
    size_t len;
  } uv;
} Udata;

// Function prototype. A script file is also a function.
typedef struct Proto {
  GCHeaderFields;

  // Constant table.
  TValue *k;
  int kSize;

  Instruction *code;
  int codeSize;

  // Functions defined inside this function.
  struct Proto **inners;
  int pSize;

  // An int-to-int map from opcodes to source lines.
  int *lineInfo;
  int lineInfoSize;
  int lineDefined;
  int lineDefinedLast;

  struct LocVar *locVars;
  int locVarsSize;

  // Upvalue names.
  TString **upvalues;
  int upvaluesSize;

  TString *source;

  GCObject *gcList;

  lu_byte upvalueNum;
  lu_byte paramNum;
  lu_byte varargMode;
  lu_byte maxStackSize;
} Proto;

/* masks for new-style vararg */
#define VARARG_HAS_ARG 0b001
#define VARARG_IS_VARARG 0b010
#define VARARG_NEEDS_ARG 0b100

typedef struct LocVar {
  TString *varname;
  // First point where variable is active.
  int startPC;
  // First point where variable is dead.
  int endPC;
} LocVar;

typedef struct UpVal {
  GCHeaderFields;
  // Points to stack or to its own value.
  TValue *v;
  union {
    // The value (when closed).
    TValue value;
    // Double linked list (when open).
    struct {
      struct UpVal *prev;
      struct UpVal *next;
    } l;
  } u;
} UpVal;

#define ClosureHeader                                                          \
  GCHeaderFields;                                                              \
  lu_byte isC;                                                                 \
  lu_byte nupvalues;                                                           \
  GCObject *gclist;                                                            \
  struct Table *env

typedef struct CClosure {
  ClosureHeader;
  lua_CFunction f;
  TValue upvalue[1];
} CClosure;

typedef struct LClosure {
  ClosureHeader;
  struct Proto *p;
  UpVal *upvalues[1];
} LClosure;

typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;

#define IS_C_FUNCTION(o)                                                       \
  (GET_TYPE(o) == LUA_TYPE_FUNCTION && CLOSURE_VALUE(o)->c.isC)
#define IS_LUA_FUNCTION(o)                                                     \
  (GET_TYPE(o) == LUA_TYPE_FUNCTION && !CLOSURE_VALUE(o)->c.isC)

/*
** Tables
*/

typedef union TKey {
  struct {
    TValueFields;
    struct Node *next; /* for chaining */
  } nk;
  TValue tvk;
} TKey;

typedef struct Node {
  TValue i_val;
  TKey i_key;
} Node;

typedef struct Table {
  GCHeaderFields;
  lu_byte flags;     /* 1<<p means tagmethod(p) is not present */
  lu_byte lsizenode; /* log2 of size of `node' array */
  struct Table *metatable;
  TValue *array; /* array part */
  Node *node;
  Node *lastfree; /* any free position is before this position */
  GCObject *gclist;
  int sizearray; /* size of `array' array */
} Table;

/*
** `module' operation for hashing (size is always a power of 2)
*/
#define lmod(s, size)                                                          \
  (CHECK_EXPR((size & (size - 1)) == 0, (cast(int, (s) & ((size) - 1)))))

#define twoto(x) (1 << (x))
#define sizenode(t) (twoto((t)->lsizenode))

#define luaO_nilobject (&luaO_nilobject_)

LUAI_DATA const TValue luaO_nilobject_;

#define ceillog2(x) (luaO_log2((x) - 1) + 1)

LUAI_FUNC int luaO_log2(unsigned int x);
LUAI_FUNC int luaO_int2fb(unsigned int x);
LUAI_FUNC int luaO_fb2int(int x);
LUAI_FUNC int luaO_rawequalObj(const TValue *t1, const TValue *t2);
LUAI_FUNC int luaO_str2d(const char *s, lua_Number *result);
LUAI_FUNC const char *luaO_pushvfstring(lua_State *L, const char *fmt,
                                        va_list argp);
LUAI_FUNC const char *luaO_pushfstring(lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid(char *out, const char *source, size_t len);
