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
#define LUA_TPROTO (LAST_TAG + 1)
#define LUA_TUPVAL (LAST_TAG + 2)
#define LUA_TDEADKEY (LAST_TAG + 3)

/*
** Union of all collectable objects
*/
typedef union GCObject GCObject;

/*
** Common Header for all collectable objects (in macro form, to be
** included in other objects)
*/
#define CommonHeader                                                           \
  GCObject *next;                                                              \
  lu_byte tt;                                                                  \
  lu_byte marked

/*
** Common header in struct form
*/
typedef struct GCHeader {
  CommonHeader;
} GCHeader;

/*
** Union of all Lua values
*/
typedef union {
  GCObject *gc;
  void *p;
  lua_Number n;
  int b;
} Value;

#define TValuefields                                                           \
  Value value;                                                                 \
  int tt

// Tagged Values.
typedef struct lua_TValue {
  TValuefields;
} TValue;

#define IS_TYPE_NIL(o) (ttype(o) == LUA_TYPE_NIL)
#define IS_TYPE_NUMBER(o) (ttype(o) == LUA_TYPE_NUMBER)
#define IS_TYPE_STRING(o) (ttype(o) == LUA_TYPE_STRING)
#define IS_TYPE_TABLE(o) (ttype(o) == LUA_TYPE_TABLE)
#define IS_TYPE_FUNCTION(o) (ttype(o) == LUA_TYPE_FUNCTION)
#define IS_TYPE_BOOLEAN(o) (ttype(o) == LUA_TYPE_BOOLEAN)
#define IS_TYPE_USERDATA(o) (ttype(o) == LUA_TYPE_USERDATA)
#define IS_TYPE_THREAD(o) (ttype(o) == LUA_TYPE_THREAD)
#define IS_TYPE_LIGHTUSERDATA(o) (ttype(o) == LUA_TYPE_LIGHTUSERDATA)

/* Macros to access values */
#define ttype(o) ((o)->tt)
#define gcvalue(o) check_exp(iscollectable(o), (o)->value.gc)
#define pvalue(o) check_exp(IS_TYPE_LIGHTUSERDATA(o), (o)->value.p)
#define nvalue(o) check_exp(IS_TYPE_NUMBER(o), (o)->value.n)
#define rawtsvalue(o) check_exp(IS_TYPE_STRING(o), &(o)->value.gc->ts)
#define tsvalue(o) (&rawtsvalue(o)->tsv)
#define rawuvalue(o) check_exp(IS_TYPE_USERDATA(o), &(o)->value.gc->u)
#define uvalue(o) (&rawuvalue(o)->uv)
#define clvalue(o) check_exp(IS_TYPE_FUNCTION(o), &(o)->value.gc->cl)
#define hvalue(o) check_exp(IS_TYPE_TABLE(o), &(o)->value.gc->h)
#define bvalue(o) check_exp(IS_TYPE_BOOLEAN(o), (o)->value.b)
#define thvalue(o) check_exp(IS_TYPE_THREAD(o), &(o)->value.gc->th)

#define l_isfalse(o) (IS_TYPE_NIL(o) || (IS_TYPE_BOOLEAN(o) && bvalue(o) == 0))

/*
** for internal debug only
*/
#define checkconsistency(obj)                                                  \
  lua_assert(!iscollectable(obj) || (ttype(obj) == (obj)->value.gc->gch.tt))

#define checkliveness(g, obj)                                                  \
  lua_assert(!iscollectable(obj) ||                                            \
             ((ttype(obj) == (obj)->value.gc->gch.tt) &&                       \
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
    i_o->tt = LUA_TYPE_LIGHTUSERDATA;                                          \
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
    checkliveness(G(L), i_o);                                                  \
  } while (0)

#define setuvalue(L, obj, x)                                                   \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_USERDATA;                                               \
    checkliveness(G(L), i_o);                                                  \
  } while (0)

#define setthvalue(L, obj, x)                                                  \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_THREAD;                                                 \
    checkliveness(G(L), i_o);                                                  \
  } while (0)

#define setclvalue(L, obj, x)                                                  \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_FUNCTION;                                               \
    checkliveness(G(L), i_o);                                                  \
  } while (0)

#define sethvalue(L, obj, x)                                                   \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TYPE_TABLE;                                                  \
    checkliveness(G(L), i_o);                                                  \
  } while (0)

#define setptvalue(L, obj, x)                                                  \
  do {                                                                         \
    TValue *i_o = (obj);                                                       \
    i_o->value.gc = cast(GCObject *, (x));                                     \
    i_o->tt = LUA_TPROTO;                                                      \
    checkliveness(G(L), i_o);                                                  \
  } while (0)

#define setobj(L, obj1, obj2)                                                  \
  do {                                                                         \
    const TValue *o2 = (obj2);                                                 \
    TValue *o1 = (obj1);                                                       \
    o1->value = o2->value;                                                     \
    o1->tt = o2->tt;                                                           \
    checkliveness(G(L), o1);                                                   \
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

#define setttype(obj, tt) (ttype(obj) = (tt))

#define iscollectable(o) (ttype(o) >= LUA_TYPE_STRING)

typedef TValue *StkId; /* index to stack elements */

/*
** String headers for string table
*/
typedef union TString {
  L_Umaxalign dummy; /* ensures maximum alignment for strings */
  struct {
    CommonHeader;
    lu_byte reserved;
    unsigned int hash;
    size_t len;
  } tsv;
} TString;

#define getstr(ts) cast(const char *, (ts) + 1)
#define svalue(o) getstr(rawtsvalue(o))

typedef union Udata {
  L_Umaxalign dummy; /* ensures maximum alignment for `local' udata */
  struct {
    CommonHeader;
    struct Table *metatable;
    struct Table *env;
    size_t len;
  } uv;
} Udata;

// Function prototype. A script file is also a function.
typedef struct Proto {
  CommonHeader;

  // Constant table.
  TValue *k;
  int kSize;

  Instruction *code;
  int codeSize;

  // Functions defined inside this function.
  struct Proto **p;
  int pSize;

  // An int-to-int map from opcodes to source lines.
  int *lineInfo;
  int lineInfoSize;

  struct LocVar *locVars;
  int locVarsSize;

  // Upvalue names.
  TString **upvalues;
  int sizeUpvalues;

  TString *source;

  int linedefined;
  int lastlinedefined;
  GCObject *gclist;

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
  // first point where variable is active.
  int startPC;
  // first point where variable is dead.
  int endPC;
} LocVar;

typedef struct UpVal {
  CommonHeader;
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
  CommonHeader;                                                                \
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
  UpVal *upvals[1];
} LClosure;

typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;

#define IS_C_FUNCTION(o) (ttype(o) == LUA_TYPE_FUNCTION && clvalue(o)->c.isC)
#define IS_LUA_FUNCTION(o) (ttype(o) == LUA_TYPE_FUNCTION && !clvalue(o)->c.isC)

/*
** Tables
*/

typedef union TKey {
  struct {
    TValuefields;
    struct Node *next; /* for chaining */
  } nk;
  TValue tvk;
} TKey;

typedef struct Node {
  TValue i_val;
  TKey i_key;
} Node;

typedef struct Table {
  CommonHeader;
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
  (check_exp((size & (size - 1)) == 0, (cast(int, (s) & ((size) - 1)))))

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
