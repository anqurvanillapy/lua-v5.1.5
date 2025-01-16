/* Limits, basic types, and some other 'installation-dependent' definitions. */

#pragma once

#include <limits.h>
#include <stddef.h>

#include "lua.h"

typedef LUAI_UINT32 lu_int32;

typedef LUAI_UMEM lu_mem;

typedef LUAI_MEM l_mem;

#define MAX_SIZET ((size_t)(~(size_t)0) - 2)

#define MAX_LUMEM ((lu_mem)(~(lu_mem)0) - 2)

#define MAX_INT (INT_MAX - 2) /* maximum value of an int (-2 for safety) */

/*
** conversion of pointer to integer
** this is for hashing only; there is no problem if the integer
** cannot hold the whole pointer value
*/
#define IntPoint(p) ((unsigned int)(lu_mem)(p))

// Type to ensure maximum alignment, this is used for reducing memory
// fragmentation.
typedef double Padding;

/* result of a 'usual argument conversion' over lua_Number */
typedef LUAI_UACNUMBER l_uacNumber;

/* internal assertions for in-house debugging */
#include <assert.h>
#define DEBUG_ASSERT(c) assert(c)
#define CHECK_EXPR(c, e) (DEBUG_ASSERT(c), (e))
#define api_check(l, e) DEBUG_ASSERT(e)

#ifndef cast
#define cast(t, exp) ((t)(exp))
#endif

#define cast_byte(i) cast(uint8_t, (i))
#define cast_num(i) cast(lua_Number, (i))
#define cast_int(i) cast(int, (i))

/*
** type for virtual-machine instructions
** must be an unsigned with (at least) 4 bytes (see details in lopcodes.h)
*/
typedef lu_int32 Instruction;

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
