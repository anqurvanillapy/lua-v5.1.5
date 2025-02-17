// Garbage collector.

#pragma once

#include "object.h"

/*
** Possible states of the Garbage Collector
*/
#define GCSpause 0
#define GCSpropagate 1
#define GCSsweepstring 2
#define GCSsweep 3
#define GCSfinalize 4

/*
** some userful bit tricks
*/
#define resetbits(x, m) ((x) &= cast(uint8_t, ~(m)))
#define setbits(x, m) ((x) |= (m))
#define testbits(x, m) ((x) & (m))
#define bitmask(b) (1 << (b))
#define bit2mask(b1, b2) (bitmask(b1) | bitmask(b2))
#define l_setbit(x, b) setbits(x, bitmask(b))
#define resetbit(x, b) resetbits(x, bitmask(b))
#define testbit(x, b) testbits(x, bitmask(b))
#define set2bits(x, b1, b2) setbits(x, (bit2mask(b1, b2)))
#define reset2bits(x, b1, b2) resetbits(x, (bit2mask(b1, b2)))
#define test2bits(x, b1, b2) testbits(x, (bit2mask(b1, b2)))

/*
** Layout for the bit use in 'marked' field:
** bit 0 - object is white (type 0)
** bit 1 - object is white (type 1)
** bit 2 - object is black
** bit 3 - for userdata: has been finalized
** bit 3 - for tables: has weak keys
** bit 4 - for tables: has weak values
** bit 5 - object is fixed (should not be collected)
** bit 6 - object is "super" fixed (only the main thread)
*/

#define WHITE0BIT 0
#define WHITE1BIT 1
#define BLACKBIT 2
#define FINALIZEDBIT 3
#define KEYWEAKBIT 3
#define VALUEWEAKBIT 4
#define FIXEDBIT 5
#define SFIXEDBIT 6
#define WHITEBITS bit2mask(WHITE0BIT, WHITE1BIT)

#define iswhite(x) test2bits((x)->gch.marked, WHITE0BIT, WHITE1BIT)
#define isblack(x) testbit((x)->gch.marked, BLACKBIT)
#define isgray(x) (!isblack(x) && !iswhite(x))

#define otherwhite(g) (g->currentwhite ^ WHITEBITS)
#define IS_DEAD(g, v) ((v)->gch.marked & otherwhite(g) & WHITEBITS)

#define changewhite(x) ((x)->gch.marked ^= WHITEBITS)
#define gray2black(x) l_setbit((x)->gch.marked, BLACKBIT)

#define valiswhite(x) (IS_COLLECTABLE(x) && iswhite(GC_VALUE(x)))

#define luaC_white(g) cast(uint8_t, (g)->currentwhite &WHITEBITS)

#define luaC_checkGC(L)                                                        \
  do {                                                                         \
    condhardstacktests(luaD_reallocstack(L, L->stackSize - EXTRA_STACK - 1));  \
    if (G(L)->totalbytes >= G(L)->GCthreshold) {                               \
      luaC_step(L);                                                            \
    }                                                                          \
  } while (false)

#define luaC_barrier(L, p, v)                                                  \
  do {                                                                         \
    if (valiswhite(v) && isblack(LuaObjectToGCObject(p))) {                    \
      luaC_barrierf(L, LuaObjectToGCObject(p), GC_VALUE(v));                   \
    }                                                                          \
  } while (false)

#define luaC_barriert(L, t, v)                                                 \
  do {                                                                         \
    if (valiswhite(v) && isblack(LuaObjectToGCObject(t))) {                    \
      luaC_barrierback(L, t);                                                  \
    }                                                                          \
  } while (false)

#define luaC_objbarrier(L, p, o)                                               \
  do {                                                                         \
    if (iswhite(LuaObjectToGCObject(o)) && isblack(LuaObjectToGCObject(p))) {  \
      luaC_barrierf(L, LuaObjectToGCObject(p), LuaObjectToGCObject(o));        \
    }                                                                          \
  } while (false)

#define luaC_objbarriert(L, t, o)                                              \
  do {                                                                         \
    if (iswhite(LuaObjectToGCObject(o)) && isblack(LuaObjectToGCObject(t))) {  \
      luaC_barrierback(L, t);                                                  \
    }                                                                          \
  } while (false)

LUAI_FUNC size_t luaC_separateudata(lua_State *L, int all);
LUAI_FUNC void luaC_callGCTM(lua_State *L);
LUAI_FUNC void luaC_freeall(lua_State *L);
LUAI_FUNC void luaC_step(lua_State *L);
LUAI_FUNC void luaC_fullgc(lua_State *L);
LUAI_FUNC void luaC_link(lua_State *L, GCObject *o, uint8_t tt);
LUAI_FUNC void luaC_linkupval(lua_State *L, Upvalue *uv);
LUAI_FUNC void luaC_barrierf(lua_State *L, GCObject *o, GCObject *v);
LUAI_FUNC void luaC_barrierback(lua_State *L, Table *t);
