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

LUAI_FUNC int luaV_lessthan(lua_State *L, const TaggedValue *l,
                            const TaggedValue *r);
LUAI_FUNC int luaV_equalval(lua_State *L, const TaggedValue *t1,
                            const TaggedValue *t2);
LUAI_FUNC const TaggedValue *luaV_tonumber(const TaggedValue *obj,
                                           TaggedValue *n);
LUAI_FUNC int luaV_tostring(lua_State *L, StackIndex obj);
LUAI_FUNC void luaV_gettable(lua_State *L, const TaggedValue *t,
                             TaggedValue *key, StackIndex val);
LUAI_FUNC void luaV_settable(lua_State *L, const TaggedValue *t,
                             TaggedValue *key, StackIndex val);
LUAI_FUNC void luaV_execute(lua_State *L, int nexeccalls);
LUAI_FUNC void luaV_concat(lua_State *L, int total, int last);
