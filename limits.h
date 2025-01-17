/* Limits, basic types, and some other 'installation-dependent' definitions. */

#pragma once

#include <limits.h>
#include <stddef.h>

#include "lua.h"

#define MAX_INT (INT_MAX - 2) /* maximum value of an int (-2 for safety) */

/*
** conversion of pointer to integer
** this is for hashing only; there is no problem if the integer
** cannot hold the whole pointer value
*/
#define IntPoint(p) ((uint32_t)(size_t)(p))

// Type to ensure maximum alignment, this is used for reducing memory
// fragmentation.
typedef uint64_t MaxAlign;

/* internal assertions for in-house debugging */
#include <assert.h>
#define DEBUG_ASSERT(c) assert(c)
#define CHECK_EXPR(c, e) (DEBUG_ASSERT(c), (e))
#define api_check(l, e) DEBUG_ASSERT(e)

#ifndef cast
#define cast(t, exp) ((t)(exp))
#endif

/*
** type for virtual-machine instructions
** must be an unsigned with (at least) 4 bytes (see details in lopcodes.h)
*/
typedef uint32_t Instruction;

/* maximum stack for a Lua function */
#define MAXSTACK 250

/* minimum size for the string table (must be power of 2) */
#ifndef MINSTRTABSIZE
#define MINSTRTABSIZE 32
#endif

/* minimum size for string buffer */
#ifndef LUA_MINBUFFER
#define LUA_MINBUFFER 32
#endif

#ifndef lua_lock
#define lua_lock(L) ((void)0)
#define lua_unlock(L) ((void)0)
#endif

#ifndef luai_threadyield
#define luai_threadyield(L)                                                    \
  {                                                                            \
    lua_unlock(L);                                                             \
    lua_lock(L);                                                               \
  }
#endif

/*
** macro to control inclusion of some hard tests on stack reallocation
*/
#ifndef HARDSTACKTESTS
#define condhardstacktests(x) ((void)0)
#else
#define condhardstacktests(x) x
#endif
