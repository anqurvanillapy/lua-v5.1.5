/* Lua tables (hash). */

#pragma once

#include "lobject.h"

#define gnode(t, i) (&(t)->node[i])
#define gkey(n) (&(n)->i_key.nk)
#define gval(n) (&(n)->i_val)
#define gnext(n) ((n)->i_key.nk.next)

#define key2tval(n) (&(n)->i_key.tvk)

LUAI_FUNC const TaggedValue *luaH_getnum(Table *t, int key);
LUAI_FUNC TaggedValue *luaH_setnum(lua_State *L, Table *t, int key);
LUAI_FUNC const TaggedValue *luaH_getstr(Table *t, TString *key);
LUAI_FUNC TaggedValue *luaH_setstr(lua_State *L, Table *t, TString *key);
LUAI_FUNC const TaggedValue *luaH_get(Table *t, const TaggedValue *key);
LUAI_FUNC TaggedValue *luaH_set(lua_State *L, Table *t, const TaggedValue *key);
LUAI_FUNC Table *luaH_new(lua_State *L, int narray, int nhash);
LUAI_FUNC void luaH_resizearray(lua_State *L, Table *t, int nasize);
LUAI_FUNC void luaH_free(lua_State *L, Table *t);
LUAI_FUNC int luaH_next(lua_State *L, Table *t, StackIndex key);
LUAI_FUNC int luaH_getn(Table *t);

#if defined(LUA_DEBUG)
LUAI_FUNC Node *luaH_mainposition(const Table *t, const TaggedValue *key);
LUAI_FUNC int luaH_isdummy(Node *n);
#endif
