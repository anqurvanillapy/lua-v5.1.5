// Load precompiled Lua chunks.

#pragma once

#include "buffer.h"
#include "object.h"

LUAI_FUNC Prototype *luaU_undump(lua_State *L, ZIO *Z, StringBuilder *buff,
                                 const char *name);

LUAI_FUNC void luaU_header(char *h);

LUAI_FUNC int luaU_dump(lua_State *L, const Prototype *f, lua_Writer w,
                        void *data, int strip);

LUAI_FUNC void luaU_print(const Prototype *f, int full);

#define HEADER_VERSION 0x51
#define HEADER_FORMAT 0
#define HEADER_SIZE 12
