// Tables (arrays and hash maps).

#pragma once

#include "object.h"

#define gnode(t, i) (&(t)->node[i])
#define gkey(n) (&(n)->i_key.nk)
#define gval(n) (&(n)->i_val)
#define gnext(n) ((n)->i_key.nk.next)

#define key2tval(n) (&(n)->i_key.tvk)

LUAI_FUNC const Value *Table_getInteger(Table *t, int key);
LUAI_FUNC Value *Table_insertInteger(lua_State *L, Table *t, int key);
LUAI_FUNC const Value *Table_getString(Table *t, String *key);
LUAI_FUNC Value *Table_insertString(lua_State *L, Table *t, String *key);
LUAI_FUNC const Value *Table_get(Table *t, const Value *key);
LUAI_FUNC Value *Table_insert(lua_State *L, Table *t, const Value *key);
LUAI_FUNC Table *Table_new(lua_State *L, int narray, int nhash);
LUAI_FUNC void Table_resizeArray(lua_State *L, Table *t, int nasize);
LUAI_FUNC void Table_free(lua_State *L, Table *t);
LUAI_FUNC int Table_next(lua_State *L, Table *t, StackIndex key);
LUAI_FUNC int Table_getBoundary(Table *t);

#ifdef LUA_INTERNAL_TESTING
LUAI_FUNC Node *Table_internalGetBucket(const Table *t, const Value *key);
LUAI_FUNC bool Table_internalIsDummy(Node *n);
#endif
