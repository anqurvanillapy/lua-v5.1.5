// String interning pool.

#pragma once

#include "lgc.h"
#include "lstate.h"
#include "object.h"

#define sizestring(s) (sizeof(struct TString) + ((s)->len + 1) * sizeof(char))
#define sizeudata(u) (sizeof(struct Userdata) + (u)->len)

#define String_pin(s) l_setbit((s)->header.marked, FIXEDBIT)

LUAI_FUNC void String_resize(lua_State *L, int newSize);
LUAI_FUNC TString *String_intern(lua_State *L, const char *str, size_t len);

// FIXME(anqur): Why is it here?
LUAI_FUNC Userdata *Userdata_new(lua_State *L, size_t size, Table *env);

// FIXME(anqur): strlen here is bad.
#define String_internCStr(L, s) (String_intern(L, s, strlen(s)))
#define String_internLiteral(L, s)                                             \
  (String_intern(L, "" s, (sizeof(s) / sizeof(char)) - 1))
