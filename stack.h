// Stack utilities.

#pragma once

#include "buffer.h"
#include "object.h"
#include "state.h"

#define luaD_checkstack(L, n)                                                  \
  do {                                                                         \
    if ((uint8_t *)L->stackLast - (uint8_t *)L->top <=                         \
        (n) * (int)sizeof(Value)) {                                            \
      Stack_grow(L, n);                                                        \
    } else {                                                                   \
      condhardstacktests(Stack_resize(L, L->stackSize - EXTRA_STACK - 1));     \
    }                                                                          \
  } while (false)

#define incr_top(L)                                                            \
  do {                                                                         \
    luaD_checkstack(L, 1);                                                     \
    L->top++;                                                                  \
  } while (false)

#define SAVE_STACK(L, p) ((uint8_t *)(p) - (uint8_t *)L->stack)
#define RESTORE_STACK(L, n) ((Value *)((uint8_t *)L->stack + (n)))

#define SAVE_CI(L, p) ((uint8_t *)(p) - (uint8_t *)L->baseCI)
#define RESTORE_CI(L, n) ((CallInfo *)((uint8_t *)L->baseCI + (n)))

typedef enum PreCallResult {
  // Call to a Lua function is ready.
  PCR_LUA_READY,
  // Call to a C function is finished.
  PCR_C_DONE,
  // C function yielded.
  PCR_C_YIELD,
} PreCallResult;

// Exceptions thrown from this function will be caught.
typedef void (*ProtectedFunc)(lua_State *L, void *ud);

LUAI_FUNC lua_Status Stack_protectedParse(lua_State *L, ZIO *z,
                                          const char *name);
LUAI_FUNC void Stack_callHook(lua_State *L, int event, int line);
LUAI_FUNC PreCallResult Stack_preCall(lua_State *L, StackIndex func,
                                      int nresults);
LUAI_FUNC void luaD_call(lua_State *L, StackIndex func, int nResults);
LUAI_FUNC lua_Status Stack_protectedCall(lua_State *L, ProtectedFunc func,
                                         void *u, ptrdiff_t oldTop,
                                         ptrdiff_t ef);
LUAI_FUNC int Stack_postCall(lua_State *L, StackIndex firstResult);
LUAI_FUNC void Stack_resizeCI(lua_State *L, int newSize);
LUAI_FUNC void Stack_resize(lua_State *L, int newsize);
LUAI_FUNC void Stack_grow(lua_State *L, int n);

[[noreturn]] LUAI_FUNC void Stack_throw(lua_State *L, lua_Status errcode);
LUAI_FUNC lua_Status Stack_rawrUnprotected(lua_State *L, ProtectedFunc f,
                                           void *ud);

LUAI_FUNC void Stack_setErrorObj(lua_State *L, lua_Status errcode,
                                 StackIndex oldTop);
