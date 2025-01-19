#pragma once

#include "lua.h"

LUALIB_API int luaopen_base(lua_State *L);
LUALIB_API int luaopen_table(lua_State *L);
LUALIB_API int luaopen_io(lua_State *L);
LUALIB_API int luaopen_os(lua_State *L);
LUALIB_API int luaopen_string(lua_State *L);
LUALIB_API int luaopen_math(lua_State *L);
LUALIB_API int luaopen_debug(lua_State *L);
LUALIB_API int luaopen_package(lua_State *L);

LUALIB_API void luaL_openlibs(lua_State *L);

#ifndef DEBUG_ASSERT
#define DEBUG_ASSERT(x) ((void)0)
#endif
