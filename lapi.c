/* Lua API. */

#include <stdarg.h>
#include <string.h>

#include "lua.h"

#include "lapi.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"
#include "lundump.h"
#include "lvm.h"

#define api_checknelems(L, n) api_check(L, (n) <= (L->top - L->base))

#define api_checkvalidindex(L, i) api_check(L, (i) != luaO_nilobject)

#define api_incr_top(L)                                                        \
  do {                                                                         \
    api_check(L, L->top < L->ci->top);                                         \
    L->top++;                                                                  \
  } while (false)

static TaggedValue *index2adr(lua_State *L, int idx) {
  if (idx > 0) {
    TaggedValue *o = L->base + (idx - 1);
    api_check(L, idx <= L->ci->top - L->base);
    if (o >= L->top) {
      return cast(TaggedValue *, luaO_nilobject);
    } else {
      return o;
    }
  } else if (idx > LUA_REGISTRYINDEX) {
    api_check(L, idx != 0 && -idx <= L->top - L->base);
    return L->top + idx;
  } else {
    switch (idx) { /* pseudo-indices */
    case LUA_REGISTRYINDEX:
      return registry(L);
    case LUA_ENVIRONINDEX: {
      Closure *func = curr_func(L);
      SET_TABLE(L, &L->env, func->c.env);
      return &L->env;
    }
    case LUA_GLOBALSINDEX:
      return gt(L);
    default: {
      Closure *func = curr_func(L);
      idx = LUA_GLOBALSINDEX - idx;
      return (idx <= func->c.nupvalues) ? &func->c.upvalue[idx - 1]
                                        : cast(TaggedValue *, luaO_nilobject);
    }
    }
  }
}

static Table *getcurrenv(lua_State *L) {
  if (L->ci == L->baseCI) {    /* no enclosing function? */
    return TABLE_VALUE(gt(L)); /* use global table as environment */
  } else {
    Closure *func = curr_func(L);
    return func->c.env;
  }
}

void luaA_pushobject(lua_State *L, const TaggedValue *o) {
  SET_OBJECT_TO_STACK(L, L->top, o);
  api_incr_top(L);
}

LUA_API int lua_checkstack(lua_State *L, int size) {
  int res = 1;
  lua_lock(L);
  if (size > LUAI_MAXCSTACK || (L->top - L->base + size) > LUAI_MAXCSTACK) {
    res = 0; /* stack overflow */
  } else if (size > 0) {
    luaD_checkstack(L, size);
    if (L->ci->top < L->top + size) {
      L->ci->top = L->top + size;
    }
  }
  lua_unlock(L);
  return res;
}

LUA_API void lua_xmove(lua_State *from, lua_State *to, int n) {
  int i;
  if (from == to) {
    return;
  }
  lua_lock(to);
  api_checknelems(from, n);
  api_check(from, G(from) == G(to));
  api_check(from, to->ci->top - to->top >= n);
  from->top -= n;
  for (i = 0; i < n; i++) {
    SET_OBJECT_TO_STACK(to, to->top++, from->top + i);
  }
  lua_unlock(to);
}

LUA_API void lua_setlevel(lua_State *from, lua_State *to) {
  to->nestedCCallNum = from->nestedCCallNum;
}

LUA_API lua_CFunction lua_atpanic(lua_State *L, lua_CFunction panicf) {
  lua_CFunction old;
  lua_lock(L);
  old = G(L)->panic;
  G(L)->panic = panicf;
  lua_unlock(L);
  return old;
}

LUA_API lua_State *lua_newthread(lua_State *L) {
  lua_State *L1;
  lua_lock(L);
  luaC_checkGC(L);
  L1 = luaE_newthread(L);
  SET_THREAD(L, L->top, L1);
  api_incr_top(L);
  lua_unlock(L);
  luai_userstatethread(L, L1);
  return L1;
}

/*
** basic stack manipulation
*/

LUA_API int lua_gettop(lua_State *L) { return cast_int(L->top - L->base); }

LUA_API void lua_settop(lua_State *L, int idx) {
  lua_lock(L);
  if (idx >= 0) {
    api_check(L, idx <= L->stackLast - L->base);
    while (L->top < L->base + idx) {
      SET_NIL(L->top++);
    }
    L->top = L->base + idx;
  } else {
    api_check(L, -(idx + 1) <= (L->top - L->base));
    L->top += idx + 1; /* `subtract' index (index is negative) */
  }
  lua_unlock(L);
}

LUA_API void lua_remove(lua_State *L, int idx) {
  StackIndex p;
  lua_lock(L);
  p = index2adr(L, idx);
  api_checkvalidindex(L, p);
  while (++p < L->top)
    SET_OBJECT_TO_SAME_STACK(L, p - 1, p);
  L->top--;
  lua_unlock(L);
}

LUA_API void lua_insert(lua_State *L, int idx) {
  StackIndex p;
  StackIndex q;
  lua_lock(L);
  p = index2adr(L, idx);
  api_checkvalidindex(L, p);
  for (q = L->top; q > p; q--)
    SET_OBJECT_TO_SAME_STACK(L, q, q - 1);
  SET_OBJECT_TO_SAME_STACK(L, p, L->top);
  lua_unlock(L);
}

LUA_API void lua_replace(lua_State *L, int idx) {
  StackIndex o;
  lua_lock(L);
  /* explicit test for incompatible code */
  if (idx == LUA_ENVIRONINDEX && L->ci == L->baseCI) {
    luaG_runerror(L, "no calling environment");
  }
  api_checknelems(L, 1);
  o = index2adr(L, idx);
  api_checkvalidindex(L, o);
  if (idx == LUA_ENVIRONINDEX) {
    Closure *func = curr_func(L);
    api_check(L, IS_TYPE_TABLE(L->top - 1));
    func->c.env = TABLE_VALUE(L->top - 1);
    luaC_barrier(L, func, L->top - 1);
  } else {
    SET_OBJECT(L, o, L->top - 1);
    if (idx < LUA_GLOBALSINDEX) /* function upvalue? */
      luaC_barrier(L, curr_func(L), L->top - 1);
  }
  L->top--;
  lua_unlock(L);
}

LUA_API void lua_pushvalue(lua_State *L, int idx) {
  lua_lock(L);
  SET_OBJECT_TO_STACK(L, L->top, index2adr(L, idx));
  api_incr_top(L);
  lua_unlock(L);
}

/*
** access functions (stack -> C)
*/

LUA_API int lua_type(lua_State *L, int idx) {
  StackIndex o = index2adr(L, idx);
  return (o == luaO_nilobject) ? LUA_TYPE_NONE : GET_TYPE(o);
}

LUA_API const char *lua_typename(lua_State *L, int t) {
  (void)L;
  return (t == LUA_TYPE_NONE) ? "no value" : luaT_typenames[t];
}

LUA_API int lua_iscfunction(lua_State *L, int idx) {
  StackIndex o = index2adr(L, idx);
  return IS_C_FUNCTION(o);
}

LUA_API int lua_isnumber(lua_State *L, int idx) {
  TaggedValue n;
  const TaggedValue *o = index2adr(L, idx);
  return tonumber(o, &n);
}

LUA_API int lua_isstring(lua_State *L, int idx) {
  int t = lua_type(L, idx);
  return (t == LUA_TYPE_STRING || t == LUA_TYPE_NUMBER);
}

LUA_API int lua_isuserdata(lua_State *L, int idx) {
  const TaggedValue *o = index2adr(L, idx);
  return (IS_TYPE_USERDATA(o) || IS_TYPE_PTR(o));
}

LUA_API int lua_rawequal(lua_State *L, int index1, int index2) {
  StackIndex o1 = index2adr(L, index1);
  StackIndex o2 = index2adr(L, index2);
  return (o1 == luaO_nilobject || o2 == luaO_nilobject)
             ? 0
             : luaO_rawequalObj(o1, o2);
}

LUA_API int lua_equal(lua_State *L, int index1, int index2) {
  StackIndex o1, o2;
  int i;
  lua_lock(L); /* may call tag method */
  o1 = index2adr(L, index1);
  o2 = index2adr(L, index2);
  i = (o1 == luaO_nilobject || o2 == luaO_nilobject) ? 0 : equalobj(L, o1, o2);
  lua_unlock(L);
  return i;
}

LUA_API int lua_lessthan(lua_State *L, int index1, int index2) {
  StackIndex o1, o2;
  int i;
  lua_lock(L); /* may call tag method */
  o1 = index2adr(L, index1);
  o2 = index2adr(L, index2);
  i = (o1 == luaO_nilobject || o2 == luaO_nilobject) ? 0
                                                     : luaV_lessthan(L, o1, o2);
  lua_unlock(L);
  return i;
}

LUA_API lua_Number lua_tonumber(lua_State *L, int idx) {
  TaggedValue n;
  const TaggedValue *o = index2adr(L, idx);
  if (tonumber(o, &n)) {
    return NUMBER_VALUE(o);
  } else {
    return 0;
  }
}

LUA_API lua_Integer lua_tointeger(lua_State *L, int idx) {
  TaggedValue n;
  const TaggedValue *o = index2adr(L, idx);
  if (tonumber(o, &n)) {
    lua_Integer res;
    lua_Number num = NUMBER_VALUE(o);
    lua_number2integer(res, num);
    return res;
  } else {
    return 0;
  }
}

LUA_API int lua_toboolean(lua_State *L, int idx) {
  const TaggedValue *o = index2adr(L, idx);
  return !IS_FALSE(o);
}

LUA_API const char *lua_tolstring(lua_State *L, int idx, size_t *len) {
  StackIndex o = index2adr(L, idx);
  if (!IS_TYPE_STRING(o)) {
    lua_lock(L);                /* `luaV_tostring' may create a new string */
    if (!luaV_tostring(L, o)) { /* conversion failed? */
      if (len != NULL) {
        *len = 0;
      }
      lua_unlock(L);
      return NULL;
    }
    luaC_checkGC(L);
    o = index2adr(L, idx); /* previous call may reallocate the stack */
    lua_unlock(L);
  }
  if (len != NULL) {
    *len = STRING_VALUE(o)->len;
  }
  return GET_STR_VALUE(o);
}

LUA_API size_t lua_objlen(lua_State *L, int idx) {
  StackIndex o = index2adr(L, idx);
  switch (GET_TYPE(o)) {
  case LUA_TYPE_STRING:
    return STRING_VALUE(o)->len;
  case LUA_TYPE_USERDATA:
    return USERDATA_VALUE(o)->len;
  case LUA_TYPE_TABLE:
    return luaH_getn(TABLE_VALUE(o));
  case LUA_TYPE_NUMBER: {
    size_t l;
    lua_lock(L); /* `luaV_tostring' may create a new string */
    l = (luaV_tostring(L, o) ? STRING_VALUE(o)->len : 0);
    lua_unlock(L);
    return l;
  }
  default:
    return 0;
  }
}

LUA_API lua_CFunction lua_tocfunction(lua_State *L, int idx) {
  StackIndex o = index2adr(L, idx);
  return (!IS_C_FUNCTION(o)) ? NULL : CLOSURE_VALUE(o)->c.f;
}

LUA_API void *lua_touserdata(lua_State *L, int idx) {
  StackIndex o = index2adr(L, idx);
  switch (GET_TYPE(o)) {
  case LUA_TYPE_USERDATA:
    return (RAW_USERDATA_VALUE(o) + 1);
  case LUA_TYPE_PTR:
    return PTR_VALUE(o);
  default:
    return NULL;
  }
}

LUA_API lua_State *lua_tothread(lua_State *L, int idx) {
  StackIndex o = index2adr(L, idx);
  return (!IS_TYPE_THREAD(o)) ? NULL : THREAD_VALUE(o);
}

LUA_API const void *lua_topointer(lua_State *L, int idx) {
  StackIndex o = index2adr(L, idx);
  switch (GET_TYPE(o)) {
  case LUA_TYPE_TABLE:
    return TABLE_VALUE(o);
  case LUA_TYPE_FUNCTION:
    return CLOSURE_VALUE(o);
  case LUA_TYPE_THREAD:
    return THREAD_VALUE(o);
  case LUA_TYPE_USERDATA:
  case LUA_TYPE_PTR:
    return lua_touserdata(L, idx);
  default:
    return NULL;
  }
}

/*
** push functions (C -> stack)
*/

LUA_API void lua_pushnil(lua_State *L) {
  lua_lock(L);
  SET_NIL(L->top);
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API void lua_pushnumber(lua_State *L, lua_Number n) {
  lua_lock(L);
  SET_NUMBER(L->top, n);
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API void lua_pushinteger(lua_State *L, lua_Integer n) {
  lua_lock(L);
  SET_NUMBER(L->top, cast_num(n));
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API void lua_pushlstring(lua_State *L, const char *s, size_t len) {
  lua_lock(L);
  luaC_checkGC(L);
  SET_STRING_TO_STACK(L, L->top, luaS_newlstr(L, s, len));
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API void lua_pushstring(lua_State *L, const char *s) {
  if (s == NULL) {
    lua_pushnil(L);
  } else {
    lua_pushlstring(L, s, strlen(s));
  }
}

LUA_API const char *lua_pushvfstring(lua_State *L, const char *fmt,
                                     va_list argp) {
  const char *ret;
  lua_lock(L);
  luaC_checkGC(L);
  ret = luaO_pushvfstring(L, fmt, argp);
  lua_unlock(L);
  return ret;
}

LUA_API const char *lua_pushfstring(lua_State *L, const char *fmt, ...) {
  const char *ret;
  va_list argp;
  lua_lock(L);
  luaC_checkGC(L);
  va_start(argp, fmt);
  ret = luaO_pushvfstring(L, fmt, argp);
  va_end(argp);
  lua_unlock(L);
  return ret;
}

LUA_API void lua_pushcclosure(lua_State *L, lua_CFunction fn, int n) {
  Closure *cl;
  lua_lock(L);
  luaC_checkGC(L);
  api_checknelems(L, n);
  cl = luaF_newCclosure(L, n, getcurrenv(L));
  cl->c.f = fn;
  L->top -= n;
  while (n--) {
    SET_OBJECT_TO_NEW(L, &cl->c.upvalue[n], L->top + n);
  }
  SET_CLOSURE(L, L->top, cl);
  lua_assert(iswhite(LuaObjectToGCObject(cl)));
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API void lua_pushboolean(lua_State *L, int b) {
  lua_lock(L);
  SET_BOOL(L->top, (b != 0)); /* ensure that true is 1 */
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API void lua_pushlightuserdata(lua_State *L, void *p) {
  lua_lock(L);
  SET_PTR(L->top, p);
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API int lua_pushthread(lua_State *L) {
  lua_lock(L);
  SET_THREAD(L, L->top, L);
  api_incr_top(L);
  lua_unlock(L);
  return (G(L)->mainthread == L);
}

/*
** get functions (Lua -> stack)
*/

LUA_API void lua_gettable(lua_State *L, int idx) {
  StackIndex t;
  lua_lock(L);
  t = index2adr(L, idx);
  api_checkvalidindex(L, t);
  luaV_gettable(L, t, L->top - 1, L->top - 1);
  lua_unlock(L);
}

LUA_API void lua_getfield(lua_State *L, int idx, const char *k) {
  StackIndex t;
  TaggedValue key;
  lua_lock(L);
  t = index2adr(L, idx);
  api_checkvalidindex(L, t);
  SET_STRING(L, &key, luaS_new(L, k));
  luaV_gettable(L, t, &key, L->top);
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API void lua_rawget(lua_State *L, int idx) {
  StackIndex t;
  lua_lock(L);
  t = index2adr(L, idx);
  api_check(L, IS_TYPE_TABLE(t));
  SET_OBJECT_TO_STACK(L, L->top - 1, luaH_get(TABLE_VALUE(t), L->top - 1));
  lua_unlock(L);
}

LUA_API void lua_rawgeti(lua_State *L, int idx, int n) {
  StackIndex o;
  lua_lock(L);
  o = index2adr(L, idx);
  api_check(L, IS_TYPE_TABLE(o));
  SET_OBJECT_TO_STACK(L, L->top, luaH_getnum(TABLE_VALUE(o), n));
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API void lua_createtable(lua_State *L, int narray, int nrec) {
  lua_lock(L);
  luaC_checkGC(L);
  SET_TABLE(L, L->top, luaH_new(L, narray, nrec));
  api_incr_top(L);
  lua_unlock(L);
}

LUA_API int lua_getmetatable(lua_State *L, int objindex) {
  const TaggedValue *obj;
  Table *mt = NULL;
  int res;
  lua_lock(L);
  obj = index2adr(L, objindex);
  switch (GET_TYPE(obj)) {
  case LUA_TYPE_TABLE:
    mt = TABLE_VALUE(obj)->metatable;
    break;
  case LUA_TYPE_USERDATA:
    mt = USERDATA_VALUE(obj)->metatable;
    break;
  default:
    mt = G(L)->mt[GET_TYPE(obj)];
    break;
  }
  if (mt == NULL) {
    res = 0;
  } else {
    SET_TABLE(L, L->top, mt);
    api_incr_top(L);
    res = 1;
  }
  lua_unlock(L);
  return res;
}

LUA_API void lua_getfenv(lua_State *L, int idx) {
  StackIndex o;
  lua_lock(L);
  o = index2adr(L, idx);
  api_checkvalidindex(L, o);
  switch (GET_TYPE(o)) {
  case LUA_TYPE_FUNCTION:
    SET_TABLE(L, L->top, CLOSURE_VALUE(o)->c.env);
    break;
  case LUA_TYPE_USERDATA:
    SET_TABLE(L, L->top, USERDATA_VALUE(o)->env);
    break;
  case LUA_TYPE_THREAD:
    SET_OBJECT_TO_STACK(L, L->top, gt(THREAD_VALUE(o)));
    break;
  default:
    SET_NIL(L->top);
    break;
  }
  api_incr_top(L);
  lua_unlock(L);
}

/*
** set functions (stack -> Lua)
*/

LUA_API void lua_settable(lua_State *L, int idx) {
  StackIndex t;
  lua_lock(L);
  api_checknelems(L, 2);
  t = index2adr(L, idx);
  api_checkvalidindex(L, t);
  luaV_settable(L, t, L->top - 2, L->top - 1);
  L->top -= 2; /* pop index and value */
  lua_unlock(L);
}

LUA_API void lua_setfield(lua_State *L, int idx, const char *k) {
  StackIndex t;
  TaggedValue key;
  lua_lock(L);
  api_checknelems(L, 1);
  t = index2adr(L, idx);
  api_checkvalidindex(L, t);
  SET_STRING(L, &key, luaS_new(L, k));
  luaV_settable(L, t, &key, L->top - 1);
  L->top--; /* pop value */
  lua_unlock(L);
}

LUA_API void lua_rawset(lua_State *L, int idx) {
  StackIndex t;
  lua_lock(L);
  api_checknelems(L, 2);
  t = index2adr(L, idx);
  api_check(L, IS_TYPE_TABLE(t));
  SET_OBJECT_TO_TABLE(L, luaH_set(L, TABLE_VALUE(t), L->top - 2), L->top - 1);
  luaC_barriert(L, TABLE_VALUE(t), L->top - 1);
  L->top -= 2;
  lua_unlock(L);
}

LUA_API void lua_rawseti(lua_State *L, int idx, int n) {
  StackIndex o;
  lua_lock(L);
  api_checknelems(L, 1);
  o = index2adr(L, idx);
  api_check(L, IS_TYPE_TABLE(o));
  SET_OBJECT_TO_TABLE(L, luaH_setnum(L, TABLE_VALUE(o), n), L->top - 1);
  luaC_barriert(L, TABLE_VALUE(o), L->top - 1);
  L->top--;
  lua_unlock(L);
}

LUA_API int lua_setmetatable(lua_State *L, int objindex) {
  TaggedValue *obj;
  Table *mt;
  lua_lock(L);
  api_checknelems(L, 1);
  obj = index2adr(L, objindex);
  api_checkvalidindex(L, obj);
  if (IS_TYPE_NIL(L->top - 1)) {
    mt = NULL;
  } else {
    api_check(L, IS_TYPE_TABLE(L->top - 1));
    mt = TABLE_VALUE(L->top - 1);
  }
  switch (GET_TYPE(obj)) {
  case LUA_TYPE_TABLE: {
    TABLE_VALUE(obj)->metatable = mt;
    if (mt)
      luaC_objbarriert(L, TABLE_VALUE(obj), mt);
    break;
  }
  case LUA_TYPE_USERDATA: {
    USERDATA_VALUE(obj)->metatable = mt;
    if (mt)
      luaC_objbarrier(L, RAW_USERDATA_VALUE(obj), mt);
    break;
  }
  default: {
    G(L)->mt[GET_TYPE(obj)] = mt;
    break;
  }
  }
  L->top--;
  lua_unlock(L);
  return 1;
}

LUA_API int lua_setfenv(lua_State *L, int idx) {
  StackIndex o;
  int res = 1;
  lua_lock(L);
  api_checknelems(L, 1);
  o = index2adr(L, idx);
  api_checkvalidindex(L, o);
  api_check(L, IS_TYPE_TABLE(L->top - 1));
  switch (GET_TYPE(o)) {
  case LUA_TYPE_FUNCTION:
    CLOSURE_VALUE(o)->c.env = TABLE_VALUE(L->top - 1);
    break;
  case LUA_TYPE_USERDATA:
    USERDATA_VALUE(o)->env = TABLE_VALUE(L->top - 1);
    break;
  case LUA_TYPE_THREAD:
    SET_TABLE(L, gt(THREAD_VALUE(o)), TABLE_VALUE(L->top - 1));
    break;
  default:
    res = 0;
    break;
  }
  if (res)
    luaC_objbarrier(L, GC_VALUE(o), TABLE_VALUE(L->top - 1));
  L->top--;
  lua_unlock(L);
  return res;
}

/*
** `load' and `call' functions (run Lua code)
*/

#define adjustresults(L, nres)                                                 \
  {                                                                            \
    if (nres == LUA_MULTRET && L->top >= L->ci->top)                           \
      L->ci->top = L->top;                                                     \
  }

#define checkresults(L, na, nr)                                                \
  api_check(L, (nr) == LUA_MULTRET || (L->ci->top - L->top >= (nr) - (na)))

LUA_API void lua_call(lua_State *L, int nargs, int nresults) {
  StackIndex func;
  lua_lock(L);
  api_checknelems(L, nargs + 1);
  checkresults(L, nargs, nresults);
  func = L->top - (nargs + 1);
  luaD_call(L, func, nresults);
  adjustresults(L, nresults);
  lua_unlock(L);
}

/*
** Execute a protected call.
*/
struct CallS { /* data to `f_call' */
  StackIndex func;
  int nresults;
};

static void f_call(lua_State *L, void *ud) {
  struct CallS *c = cast(struct CallS *, ud);
  luaD_call(L, c->func, c->nresults);
}

LUA_API int lua_pcall(lua_State *L, int nargs, int nresults, int errfunc) {
  struct CallS c;
  int status;
  ptrdiff_t func;
  lua_lock(L);
  api_checknelems(L, nargs + 1);
  checkresults(L, nargs, nresults);
  if (errfunc == 0) {
    func = 0;
  } else {
    StackIndex o = index2adr(L, errfunc);
    api_checkvalidindex(L, o);
    func = savestack(L, o);
  }
  c.func = L->top - (nargs + 1); /* function to be called */
  c.nresults = nresults;
  status = luaD_pcall(L, f_call, &c, savestack(L, c.func), func);
  adjustresults(L, nresults);
  lua_unlock(L);
  return status;
}

/*
** Execute a protected C call.
*/
struct CCallS { /* data to `f_Ccall' */
  lua_CFunction func;
  void *ud;
};

static void f_Ccall(lua_State *L, void *ud) {
  struct CCallS *c = cast(struct CCallS *, ud);
  Closure *cl;
  cl = luaF_newCclosure(L, 0, getcurrenv(L));
  cl->c.f = c->func;
  SET_CLOSURE(L, L->top, cl); /* push function */
  api_incr_top(L);
  SET_PTR(L->top, c->ud); /* push only argument */
  api_incr_top(L);
  luaD_call(L, L->top - 2, 0);
}

LUA_API int lua_cpcall(lua_State *L, lua_CFunction func, void *ud) {
  struct CCallS c;
  int status;
  lua_lock(L);
  c.func = func;
  c.ud = ud;
  status = luaD_pcall(L, f_Ccall, &c, savestack(L, L->top), 0);
  lua_unlock(L);
  return status;
}

LUA_API int lua_load(lua_State *L, lua_Reader reader, void *data,
                     const char *chunkname) {
  ZIO z;
  int status;
  lua_lock(L);
  if (!chunkname) {
    chunkname = "?";
  }
  luaZ_init(L, &z, reader, data);
  status = luaD_protectedparser(L, &z, chunkname);
  lua_unlock(L);
  return status;
}

LUA_API int lua_dump(lua_State *L, lua_Writer writer, void *data) {
  int status;
  TaggedValue *o;
  lua_lock(L);
  api_checknelems(L, 1);
  o = L->top - 1;
  if (IS_LUA_FUNCTION(o)) {
    status = luaU_dump(L, CLOSURE_VALUE(o)->l.p, writer, data, 0);
  } else {
    status = 1;
  }
  lua_unlock(L);
  return status;
}

LUA_API int lua_status(lua_State *L) { return L->status; }

/*
** Garbage-collection function
*/

LUA_API int lua_gc(lua_State *L, int what, int data) {
  int res = 0;
  global_State *g;
  lua_lock(L);
  g = G(L);
  switch (what) {
  case LUA_GCSTOP: {
    g->GCthreshold = MAX_LUMEM;
    break;
  }
  case LUA_GCRESTART: {
    g->GCthreshold = g->totalbytes;
    break;
  }
  case LUA_GCCOLLECT: {
    luaC_fullgc(L);
    break;
  }
  case LUA_GCCOUNT: {
    /* GC values are expressed in Kbytes: #bytes/2^10 */
    res = cast_int(g->totalbytes >> 10);
    break;
  }
  case LUA_GCCOUNTB: {
    res = cast_int(g->totalbytes & 0x3ff);
    break;
  }
  case LUA_GCSTEP: {
    lu_mem a = (cast(lu_mem, data) << 10);
    if (a <= g->totalbytes) {
      g->GCthreshold = g->totalbytes - a;
    } else {
      g->GCthreshold = 0;
    }
    while (g->GCthreshold <= g->totalbytes) {
      luaC_step(L);
      if (g->gcstate == GCSpause) { /* end of cycle? */
        res = 1;                    /* signal it */
        break;
      }
    }
    break;
  }
  case LUA_GCSETPAUSE: {
    res = g->gcpause;
    g->gcpause = data;
    break;
  }
  case LUA_GCSETSTEPMUL: {
    res = g->gcstepmul;
    g->gcstepmul = data;
    break;
  }
  default:
    res = -1; /* invalid option */
  }
  lua_unlock(L);
  return res;
}

/*
** miscellaneous functions
*/

LUA_API int lua_error(lua_State *L) {
  lua_lock(L);
  api_checknelems(L, 1);
  luaG_errormsg(L);
  lua_unlock(L);
  return 0; /* to avoid warnings */
}

LUA_API int lua_next(lua_State *L, int idx) {
  StackIndex t;
  int more;
  lua_lock(L);
  t = index2adr(L, idx);
  api_check(L, IS_TYPE_TABLE(t));
  more = luaH_next(L, TABLE_VALUE(t), L->top - 1);
  if (more) {
    api_incr_top(L);
  } else {       /* no more elements */
    L->top -= 1; /* remove key */
  }
  lua_unlock(L);
  return more;
}

LUA_API void lua_concat(lua_State *L, int n) {
  lua_lock(L);
  api_checknelems(L, n);
  if (n >= 2) {
    luaC_checkGC(L);
    luaV_concat(L, n, cast_int(L->top - L->base) - 1);
    L->top -= (n - 1);
  } else if (n == 0) { /* push empty string */
    SET_STRING_TO_STACK(L, L->top, luaS_newlstr(L, "", 0));
    api_incr_top(L);
  }
  /* else n == 1; nothing to do */
  lua_unlock(L);
}

LUA_API lua_Alloc lua_getallocf(lua_State *L, void **ud) {
  lua_Alloc f;
  lua_lock(L);
  if (ud) {
    *ud = G(L)->ud;
  }
  f = G(L)->frealloc;
  lua_unlock(L);
  return f;
}

LUA_API void lua_setallocf(lua_State *L, lua_Alloc f, void *ud) {
  lua_lock(L);
  G(L)->ud = ud;
  G(L)->frealloc = f;
  lua_unlock(L);
}

LUA_API void *lua_newuserdata(lua_State *L, size_t size) {
  Udata *u;
  lua_lock(L);
  luaC_checkGC(L);
  u = luaS_newudata(L, size, getcurrenv(L));
  SET_USERDATA(L, L->top, u);
  api_incr_top(L);
  lua_unlock(L);
  return u + 1;
}

static const char *aux_upvalue(StackIndex fi, int n, TaggedValue **val) {
  Closure *f;
  if (!IS_TYPE_FUNCTION(fi)) {
    return NULL;
  }
  f = CLOSURE_VALUE(fi);
  if (f->c.isC) {
    if (!(1 <= n && n <= f->c.nupvalues)) {
      return NULL;
    }
    *val = &f->c.upvalue[n - 1];
    return "";
  } else {
    Prototype *p = f->l.p;
    if (!(1 <= n && n <= p->upvaluesSize)) {
      return NULL;
    }
    *val = f->l.upvalues[n - 1]->v;
    return GET_STR(p->upvalues[n - 1]);
  }
}

LUA_API const char *lua_getupvalue(lua_State *L, int funcindex, int n) {
  const char *name;
  TaggedValue *val;
  lua_lock(L);
  name = aux_upvalue(index2adr(L, funcindex), n, &val);
  if (name) {
    SET_OBJECT_TO_STACK(L, L->top, val);
    api_incr_top(L);
  }
  lua_unlock(L);
  return name;
}

LUA_API const char *lua_setupvalue(lua_State *L, int funcindex, int n) {
  const char *name;
  TaggedValue *val;
  StackIndex fi;
  lua_lock(L);
  fi = index2adr(L, funcindex);
  api_checknelems(L, 1);
  name = aux_upvalue(fi, n, &val);
  if (name) {
    L->top--;
    SET_OBJECT(L, val, L->top);
    luaC_barrier(L, CLOSURE_VALUE(fi), L->top);
  }
  lua_unlock(L);
  return name;
}
