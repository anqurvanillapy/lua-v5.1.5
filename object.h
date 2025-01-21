/* Type definitions for Lua objects. */

#pragma once

#include <stdarg.h>

#include "limits.h"
#include "lua.h"

// Types for values visible from Lua.
#define LAST_TYPE LUA_TYPE_THREAD
#define NUM_TYPES (LAST_TYPE + 1)

// Extra types for non-values.
#define LUA_TYPE_PROTO (LAST_TYPE + 1)
#define LUA_TYPE_UPVALUE (LAST_TYPE + 2)
#define LUA_TYPE_DEAD (LAST_TYPE + 3) // dead object to garbage collect

typedef union GCObject GCObject;

typedef struct GCHeader {
  GCObject *next;
  uint8_t tt;
  uint8_t marked;
} GCHeader;

typedef union Variant {
  GCObject *gc;
  void *p;
  double n;
  bool b;
} Variant;

typedef struct Value {
  Variant variant;
  uint8_t tt;
} Value;

#define GET_TYPE(o) ((o)->tt)
#define SET_TYPE(obj, t) ((obj)->tt = (t))

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
#define IS_COLLECTABLE(o) (GET_TYPE(o) >= LUA_TYPE_STRING)

// Getter macros.

#define GC_VALUE(o) CHECK_EXPR(IS_COLLECTABLE(o), (o)->variant.gc)
#define PTR_VALUE(o) CHECK_EXPR(IS_TYPE_PTR(o), (o)->variant.p)
#define NUMBER_VALUE(o) CHECK_EXPR(IS_TYPE_NUMBER(o), (o)->variant.n)
#define STRING_VALUE(o) CHECK_EXPR(IS_TYPE_STRING(o), &(o)->variant.gc->ts)
#define USERDATA_VALUE(o) CHECK_EXPR(IS_TYPE_USERDATA(o), &(o)->variant.gc->u)
#define CLOSURE_VALUE(o) CHECK_EXPR(IS_TYPE_FUNCTION(o), &(o)->variant.gc->cl)
#define TABLE_VALUE(o) CHECK_EXPR(IS_TYPE_TABLE(o), &(o)->variant.gc->h)
#define BOOL_VALUE(o) CHECK_EXPR(IS_TYPE_BOOLEAN(o), (o)->variant.b)
#define THREAD_VALUE(o) CHECK_EXPR(IS_TYPE_THREAD(o), &(o)->variant.gc->th)

#define DEBUG_CHECK_CONSISTENCY(obj)                                           \
  assert(!IS_COLLECTABLE(obj) || (GET_TYPE(obj) == (obj)->variant.gc->gch.tt))
#define DEBUG_CHECK_LIVENESS(g, obj)                                           \
  assert(!IS_COLLECTABLE(obj) ||                                               \
         ((GET_TYPE(obj) == (obj)->variant.gc->gch.tt) &&                      \
          !IS_DEAD(g, (obj)->variant.gc)))

// Setter macros.

#define SET_NIL(obj) ((obj)->tt = LUA_TYPE_NIL)

#define SET_NUMBER(obj, x)                                                     \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.n = (x);                                                      \
    i_o->tt = LUA_TYPE_NUMBER;                                                 \
  } while (false)

#define SET_PTR(obj, x)                                                        \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.p = (x);                                                      \
    i_o->tt = LUA_TYPE_PTR;                                                    \
  } while (false)

#define SET_BOOL(obj, x)                                                       \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.b = (x);                                                      \
    i_o->tt = LUA_TYPE_BOOLEAN;                                                \
  } while (false)

#define SET_STRING(L, obj, x)                                                  \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.gc = (GCObject *)(x);                                         \
    i_o->tt = LUA_TYPE_STRING;                                                 \
    DEBUG_CHECK_LIVENESS(G(L), i_o);                                           \
  } while (false)

#define SET_USERDATA(L, obj, x)                                                \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.gc = (GCObject *)(x);                                         \
    i_o->tt = LUA_TYPE_USERDATA;                                               \
    DEBUG_CHECK_LIVENESS(G(L), i_o);                                           \
  } while (false)

#define SET_THREAD(L, obj, x)                                                  \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.gc = (GCObject *)(x);                                         \
    i_o->tt = LUA_TYPE_THREAD;                                                 \
    DEBUG_CHECK_LIVENESS(G(L), i_o);                                           \
  } while (false)

#define SET_CLOSURE(L, obj, x)                                                 \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.gc = (GCObject *)(x);                                         \
    i_o->tt = LUA_TYPE_FUNCTION;                                               \
    DEBUG_CHECK_LIVENESS(G(L), i_o);                                           \
  } while (false)

#define SET_TABLE(L, obj, x)                                                   \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.gc = (GCObject *)(x);                                         \
    i_o->tt = LUA_TYPE_TABLE;                                                  \
    DEBUG_CHECK_LIVENESS(G(L), i_o);                                           \
  } while (false)

#define SET_PROTO(L, obj, x)                                                   \
  do {                                                                         \
    Value *i_o = (obj);                                                        \
    i_o->variant.gc = (GCObject *)(x);                                         \
    i_o->tt = LUA_TYPE_PROTO;                                                  \
    DEBUG_CHECK_LIVENESS(G(L), i_o);                                           \
  } while (false)

#define SET_OBJECT(L, obj1, obj2)                                              \
  do {                                                                         \
    const Value *o2 = (obj2);                                                  \
    Value *o1 = (obj1);                                                        \
    o1->variant = o2->variant;                                                 \
    o1->tt = o2->tt;                                                           \
    DEBUG_CHECK_LIVENESS(G(L), o1);                                            \
  } while (false)

// Different types of setters, according to the destination.

#define SET_OBJECT_TO_SAME_STACK SET_OBJECT
#define SET_OBJECT_TO_STACK SET_OBJECT
#define SET_STRING_TO_STACK SET_STRING
#define SET_TABLE_TO_STACK SET_TABLE
#define SET_PROTO_TO_STACK SET_PROTO
#define SET_TABLE_TO_TABLE SET_OBJECT
#define SET_OBJECT_TO_TABLE SET_OBJECT
#define SET_OBJECT_TO_NEW SET_OBJECT
#define SET_STRING_TO_NEW SET_STRING

typedef Value *StackIndex;

typedef struct String {
  GCHeader header;
  uint8_t keywordID;
  uint32_t hash;
  size_t len;
} String;
static_assert(alignof(String) == alignof(MaxAlign));

#define STRING_CONTENT(ts) (const char *)((ts) + 1)
#define VALUE_STRING_CONTENT(o) STRING_CONTENT(STRING_VALUE(o))

typedef struct Userdata {
  GCHeader header;
  struct Table *metatable;
  struct Table *env;
  size_t len;
} Userdata;
static_assert(alignof(Userdata) == alignof(MaxAlign));

// Function prototype. A script file is also a function.
typedef struct Prototype {
  GCHeader header;

  Value *constants;
  int constantsSize;

  Instruction *code;
  int codeSize;

  // Functions defined inside this function.
  struct Prototype **inners;
  int innersSize; // FIXME(anqur): quite hard to use `size_t` here

  String *source;
  // An int-to-int map from opcodes to source lines.
  int *lineInfo;
  int lineInfoSize;
  int lineDefined;
  int lineDefinedLast;

  struct LocVar *locVars;
  int locVarsSize;

  // Upvalue names.
  String **upvalues;
  int upvaluesSize;
  uint8_t upvaluesNum;

  GCObject *gcList;

  uint8_t paramsNum;
  uint8_t varargMode;
  uint8_t maxStackSize;
} Prototype;

/* masks for new-style vararg */
#define VARARG_HAS_ARG 0b001
#define VARARG_IS_VARARG 0b010
#define VARARG_NEEDS_ARG 0b100

typedef struct LocVar {
  String *name;
  // First point where variable is active.
  int startPC;
  // First point where variable is dead.
  int endPC;
} LocVar;

typedef struct Upvalue {
  GCHeader header;
  // Points to stack or to its own value.
  Value *v;
  union {
    // The value (when closed).
    Value value;
    // Double linked list (when open).
    struct {
      struct Upvalue *prev;
      struct Upvalue *next;
    } l;
  } u;
} Upvalue;

typedef struct ClosureHeader {
  GCHeader header;
  bool isC;
  uint8_t nupvalues;
  GCObject *gclist;
  struct Table *env;
} ClosureHeader;

typedef struct CClosure {
  ClosureHeader header;
  lua_CFunction f;
  Value upvalue[1];
} CClosure;

typedef struct LClosure {
  ClosureHeader header;
  struct Prototype *p;
  Upvalue *upvalues[1];
} LClosure;

typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;

#define IS_C_FUNCTION(o)                                                       \
  (GET_TYPE(o) == LUA_TYPE_FUNCTION && CLOSURE_VALUE(o)->c.header.isC)
#define IS_LUA_FUNCTION(o)                                                     \
  (GET_TYPE(o) == LUA_TYPE_FUNCTION && !CLOSURE_VALUE(o)->c.header.isC)

/*
** Tables
*/

typedef union TKey {
  struct {
    Variant variant;
    uint8_t tt;
    struct Node *next; /* for chaining */
  } nk;
  Value tvk;
} TKey;

typedef struct Node {
  Value i_val;
  TKey i_key;
} Node;

typedef struct Table {
  GCHeader header;
  uint8_t flags;     /* 1<<p means tagmethod(p) is not present */
  uint8_t lsizenode; /* log2 of size of 'node' array */
  struct Table *metatable;
  Value *array; /* array part */
  Node *node;
  Node *lastfree; /* any free position is before this position */
  GCObject *gclist;
  int sizearray; /* size of `array' array */
} Table;

/*
** `module' operation for hashing (size is always a power of 2)
*/
#define lmod(s, size)                                                          \
  (CHECK_EXPR((size & (size - 1)) == 0, (int)((s) & ((size) - 1))))

#define twoto(x) (1 << (x))
#define sizenode(t) (twoto((t)->lsizenode))

LUAI_DATA const Value valueNil;

#define ceillog2(x) (luaO_log2((x) - 1) + 1)

LUAI_FUNC int luaO_log2(unsigned int x);
LUAI_FUNC int luaO_int2fb(unsigned int x);
LUAI_FUNC int luaO_fb2int(int x);
LUAI_FUNC int luaO_rawequalObj(const Value *t1, const Value *t2);
LUAI_FUNC int luaO_str2d(const char *s, double *result);
LUAI_FUNC const char *luaO_pushvfstring(lua_State *L, const char *fmt,
                                        va_list argp);
LUAI_FUNC const char *luaO_pushfstring(lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid(char *out, const char *source, size_t len);
