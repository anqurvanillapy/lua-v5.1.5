// String interning pool.

#pragma once

#include "lgc.h"
#include "lstate.h"
#include "object.h"

#define sizestring(s) (sizeof(struct String) + ((s)->len + 1) * sizeof(char))
#define sizeudata(u) (sizeof(struct Userdata) + (u)->len)

#define String_intern(s) l_setbit((s)->header.marked, FIXEDBIT)

LUAI_FUNC void String_resize(lua_State *L, int newSize);
LUAI_FUNC String *String_createSized(lua_State *L, const char *str, size_t len);

// FIXME(anqur): Why is it here?
LUAI_FUNC Userdata *Userdata_new(lua_State *L, size_t size, Table *env);

// FIXME(anqur): strlen here is bad.
#define String_create(L, s) (String_createSized(L, s, strlen(s)))
#define String_createLiteral(L, s)                                             \
  (String_createSized(L, "" s, (sizeof(s) / sizeof(char)) - 1))
