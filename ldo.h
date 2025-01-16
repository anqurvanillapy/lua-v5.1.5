/* Stack and Call structure of Lua. */

#pragma once

#include "lobject.h"
#include "lstate.h"
#include "lzio.h"

#define luaD_checkstack(L, n)                                                  \
  if ((char *)L->stackLast - (char *)L->top <= (n) * (int)sizeof(Value))       \
    luaD_growstack(L, n);                                                      \
  else                                                                         \
    condhardstacktests(luaD_reallocstack(L, L->stackSize - EXTRA_STACK - 1));

#define incr_top(L)                                                            \
  {                                                                            \
    luaD_checkstack(L, 1);                                                     \
    L->top++;                                                                  \
  }

#define savestack(L, p) ((char *)(p) - (char *)L->stack)
#define restorestack(L, n) ((Value *)((char *)L->stack + (n)))

#define saveci(L, p) ((char *)(p) - (char *)L->baseCI)
#define restoreci(L, n) ((CallInfo *)((char *)L->baseCI + (n)))

/* results from luaD_precall */
#define PCRLUA 0   /* initiated a call to a Lua function */
#define PCRC 1     /* did a call to a C function */
#define PCRYIELD 2 /* C funtion yielded */

/* type of protected functions, to be ran by `runprotected' */
typedef void (*Pfunc)(lua_State *L, void *ud);

LUAI_FUNC int luaD_protectedparser(lua_State *L, ZIO *z, const char *name);
LUAI_FUNC void luaD_callhook(lua_State *L, int event, int line);
LUAI_FUNC int luaD_precall(lua_State *L, StackIndex func, int nresults);
LUAI_FUNC void luaD_call(lua_State *L, StackIndex func, int nResults);
LUAI_FUNC int luaD_pcall(lua_State *L, Pfunc func, void *u, ptrdiff_t oldtop,
                         ptrdiff_t ef);
LUAI_FUNC int luaD_poscall(lua_State *L, StackIndex firstResult);
LUAI_FUNC void luaD_reallocCI(lua_State *L, int newsize);
LUAI_FUNC void luaD_reallocstack(lua_State *L, int newsize);
LUAI_FUNC void luaD_growstack(lua_State *L, int n);

LUAI_FUNC void luaD_throw(lua_State *L, int errcode);
LUAI_FUNC int luaD_rawrunprotected(lua_State *L, Pfunc f, void *ud);

LUAI_FUNC void luaD_seterrorobj(lua_State *L, int errcode, StackIndex oldtop);
