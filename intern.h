/* String table (keep all strings handled by Lua). */

#pragma once

#include "lgc.h"
#include "lstate.h"
#include "object.h"

#define sizestring(s) (sizeof(union TString) + ((s)->len + 1) * sizeof(char))
#define sizeudata(u) (sizeof(union Userdata) + (u)->len)

#define luaS_fix(s) l_setbit((s)->tsv.header.marked, FIXEDBIT)

LUAI_FUNC void String_resize(lua_State *L, int newSize);
LUAI_FUNC TString *String_intern(lua_State *L, const char *str, size_t l);
LUAI_FUNC Userdata *luaS_newudata(lua_State *L, size_t s, Table *e);

#define luaS_new(L, s) (String_intern(L, s, strlen(s)))
#define luaS_newliteral(L, s)                                                  \
  (String_intern(L, "" s, (sizeof(s) / sizeof(char)) - 1))
