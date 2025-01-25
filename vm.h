// Virtual machine.

#pragma once

#include "object.h"
#include "stack.h"
#include "tag.h"

LUAI_FUNC bool Object_lessThan(lua_State *L, const Value *l, const Value *r);
LUAI_FUNC bool Object_equal(lua_State *L, const Value *t1, const Value *t2);
LUAI_FUNC const Value *luaV_tonumber(const Value *obj, Value *n);
LUAI_FUNC bool luaV_tostring(lua_State *L, StackIndex obj);
LUAI_FUNC void luaV_gettable(lua_State *L, const Value *t, Value *key,
                             StackIndex val);
LUAI_FUNC void luaV_settable(lua_State *L, const Value *t, Value *key,
                             StackIndex val);
LUAI_FUNC void luaV_execute(lua_State *L, int nexeccalls);
LUAI_FUNC void luaV_concat(lua_State *L, int total, int last);

#define tostring(L, o)                                                         \
  ((GET_TYPE(o) == LUA_TYPE_STRING) || (luaV_tostring(L, o)))

#define tonumber(o, n)                                                         \
  (GET_TYPE(o) == LUA_TYPE_NUMBER || (((o) = luaV_tonumber(o, n)) != nullptr))

#define equalobj(L, o1, o2)                                                    \
  (GET_TYPE(o1) == GET_TYPE(o2) && Object_equal(L, o1, o2))
