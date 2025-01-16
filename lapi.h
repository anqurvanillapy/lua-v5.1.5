/* Auxiliary functions from Lua API. */

#pragma once

#include "object.h"

LUAI_FUNC void luaA_pushobject(lua_State *L, const Value *o);
