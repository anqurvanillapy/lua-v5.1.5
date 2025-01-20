// Tag methods.

#pragma once

#include "object.h"

typedef enum {
  TM_INDEX,
  TM_NEWINDEX,
  TM_GC,
  TM_MODE,
  TM_EQ, /* last tag method with `fast' access */
  TM_ADD,
  TM_SUB,
  TM_MUL,
  TM_DIV,
  TM_MOD,
  TM_POW,
  TM_UNM,
  TM_LEN,
  TM_LT,
  TM_LE,
  TM_CONCAT,
  TM_CALL,
  TM_N /* number of elements in the enum */
} TMS;

#define GLOBAL_FAST_TM(g, et, e)                                               \
  ((et) == nullptr               ? nullptr                                     \
   : ((et)->flags & (1u << (e))) ? nullptr                                     \
                                 : luaT_gettm(et, e, (g)->tmname[e]))

#define FAST_TM(l, et, e) GLOBAL_FAST_TM(G(l), et, e)

LUAI_FUNC const Value *luaT_gettm(Table *events, TMS event, String *ename);
LUAI_FUNC const Value *luaT_gettmbyobj(lua_State *L, const Value *o, TMS event);
LUAI_FUNC void luaT_init(lua_State *L);
