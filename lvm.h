/* Lua virtual machine. */

#pragma once

#include "ldo.h"
#include "lobject.h"
#include "ltm.h"

#define tostring(L, o)                                                         \
  ((GET_TYPE(o) == LUA_TYPE_STRING) || (luaV_tostring(L, o)))

#define tonumber(o, n)                                                         \
  (GET_TYPE(o) == LUA_TYPE_NUMBER || (((o) = luaV_tonumber(o, n)) != NULL))

#define equalobj(L, o1, o2)                                                    \
  (GET_TYPE(o1) == GET_TYPE(o2) && luaV_equalval(L, o1, o2))

LUAI_FUNC int luaV_lessthan(lua_State *L, const TValue *l, const TValue *r);
LUAI_FUNC int luaV_equalval(lua_State *L, const TValue *t1, const TValue *t2);
LUAI_FUNC const TValue *luaV_tonumber(const TValue *obj, TValue *n);
LUAI_FUNC int luaV_tostring(lua_State *L, StkId obj);
LUAI_FUNC void luaV_gettable(lua_State *L, const TValue *t, TValue *key,
                             StkId val);
LUAI_FUNC void luaV_settable(lua_State *L, const TValue *t, TValue *key,
                             StkId val);
LUAI_FUNC void luaV_execute(lua_State *L, int nexeccalls);
LUAI_FUNC void luaV_concat(lua_State *L, int total, int last);
