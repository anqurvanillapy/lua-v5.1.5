/* Auxiliary functions to manipulate prototypes and closures. */

#pragma once

#include "lobject.h"

#define sizeCclosure(n)                                                        \
  (cast(int, sizeof(CClosure)) + cast(int, sizeof(Value) * ((n) - 1)))

#define sizeLclosure(n)                                                        \
  (cast(int, sizeof(LClosure)) + cast(int, sizeof(Value *) * ((n) - 1)))

LUAI_FUNC Prototype *luaF_newproto(lua_State *L);
LUAI_FUNC Closure *luaF_newCclosure(lua_State *L, int nelems, Table *e);
LUAI_FUNC Closure *luaF_newLclosure(lua_State *L, int nelems, Table *e);
LUAI_FUNC Upvalue *luaF_newupval(lua_State *L);
LUAI_FUNC Upvalue *luaF_findupval(lua_State *L, StackIndex level);
LUAI_FUNC void luaF_close(lua_State *L, StackIndex level);
LUAI_FUNC void luaF_freeproto(lua_State *L, Prototype *f);
LUAI_FUNC void luaF_freeclosure(lua_State *L, Closure *c);
LUAI_FUNC void luaF_freeupval(lua_State *L, Upvalue *uv);
LUAI_FUNC const char *luaF_getlocalname(const Prototype *func, int local_number,
                                        int pc);
