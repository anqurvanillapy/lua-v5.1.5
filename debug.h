// Debugging utilities.

#pragma once

#include "state.h"

#define pcRel(pc, p) (cast(int, (pc) - (p)->code) - 1)

#define getline(f, pc) (((f)->lineInfo) ? (f)->lineInfo[pc] : 0)

#define resethookcount(L) (L->hookCount = L->baseHookCount)

LUAI_FUNC void luaG_typeerror(lua_State *L, const Value *o, const char *opname);
LUAI_FUNC void luaG_concaterror(lua_State *L, StackIndex p1, StackIndex p2);
LUAI_FUNC void luaG_aritherror(lua_State *L, const Value *p1, const Value *p2);
LUAI_FUNC int luaG_ordererror(lua_State *L, const Value *p1, const Value *p2);
LUAI_FUNC void luaG_runerror(lua_State *L, const char *fmt, ...);
[[noreturn]] LUAI_FUNC void luaG_errormsg(lua_State *L);
LUAI_FUNC int luaG_checkcode(const Prototype *pt);
LUAI_FUNC int luaG_checkopenop(Instruction i);
LUAI_FUNC void luaA_pushobject(lua_State *L, const Value *o);
