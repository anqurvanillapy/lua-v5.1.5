// Tables (arrays and hash maps).

#pragma once

#include "object.h"

#define gnode(t, i) (&(t)->node[i])
#define gkey(n) (&(n)->i_key.nk)
#define gval(n) (&(n)->i_val)
#define gnext(n) ((n)->i_key.nk.next)

#define key2tval(n) (&(n)->i_key.tvk)

LUAI_FUNC const Value *luaH_getnum(Table *t, int key);
LUAI_FUNC Value *luaH_setnum(lua_State *L, Table *t, int key);
LUAI_FUNC const Value *luaH_getstr(Table *t, String *key);
LUAI_FUNC Value *luaH_setstr(lua_State *L, Table *t, String *key);
LUAI_FUNC const Value *luaH_get(Table *t, const Value *key);
LUAI_FUNC Value *luaH_set(lua_State *L, Table *t, const Value *key);
LUAI_FUNC Table *luaH_new(lua_State *L, int narray, int nhash);
LUAI_FUNC void luaH_resizearray(lua_State *L, Table *t, int nasize);
LUAI_FUNC void luaH_free(lua_State *L, Table *t);
LUAI_FUNC int luaH_next(lua_State *L, Table *t, StackIndex key);
LUAI_FUNC int luaH_getn(Table *t);

#ifdef LUA_INTERNAL_TESTING
LUAI_FUNC Node *luaH_mainposition(const Table *t, const Value *key);
LUAI_FUNC int luaH_isdummy(Node *n);
#endif
