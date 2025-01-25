// Closure utilities.

#pragma once

#include "object.h"

#define C_CLOSURE_SIZE(n) (sizeof(CClosure) + sizeof(Value) * ((n) - 1))
#define L_CLOSURE_SIZE(n) (sizeof(LClosure) + sizeof(Value *) * ((n) - 1))

LUAI_FUNC Prototype *Prototype_new(lua_State *L);
LUAI_FUNC Closure *Closure_newC(lua_State *L, size_t nelems, Table *e);
LUAI_FUNC Closure *Closure_newL(lua_State *L, size_t nelems, Table *e);
LUAI_FUNC Upvalue *Upvalue_new(lua_State *L);
LUAI_FUNC Upvalue *Closure_findUpvalue(lua_State *L, StackIndex level);
LUAI_FUNC void Closure_close(lua_State *L, StackIndex level);
LUAI_FUNC void Prototype_free(lua_State *L, Prototype *f);
LUAI_FUNC void Closure_free(lua_State *L, Closure *c);
LUAI_FUNC void Upvalue_free(lua_State *L, Upvalue *uv);
LUAI_FUNC const char *Prototype_getLocalName(const Prototype *f, int localNum,
                                             int pc);
