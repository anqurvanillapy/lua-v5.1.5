// Memory management.

#pragma once

#include <stddef.h>

#include "limits.h"

#define MEMERRMSG "not enough memory"

LUAI_FUNC void *luaM_realloc_(lua_State *L, void *block, size_t oldSize,
                              size_t newSize);
LUAI_FUNC void *luaM_tooBig(lua_State *L);
LUAI_FUNC void *luaM_growaux_(lua_State *L, void *block, int *size,
                              size_t elemSize, int limit, const char *errMsg);

#define luaM_reallocv(L, block, oldSize, newSize, elemSize)                    \
  ((((size_t)newSize) <= SIZE_MAX / (elemSize))                                \
       ? luaM_realloc_(L, (block), (oldSize) * (elemSize),                     \
                       (newSize) * (elemSize))                                 \
       : luaM_tooBig(L))

#define luaM_freemem(L, b, s) luaM_realloc_(L, (b), (s), 0)
#define luaM_free(L, b) luaM_realloc_(L, (b), sizeof(*(b)), 0)
#define luaM_freeArray(L, b, n, t) luaM_reallocv(L, (b), n, 0, sizeof(t))

#define luaM_malloc(L, t) luaM_realloc_(L, nullptr, 0, (t))
#define luaM_new(L, t) luaM_malloc(L, sizeof(t))
#define luaM_newvector(L, n, t) luaM_reallocv(L, nullptr, 0, n, sizeof(t))

#define luaM_growvector(L, v, nelems, size, t, limit, e)                       \
  do {                                                                         \
    if ((nelems) + 1 > (size)) {                                               \
      ((v) = luaM_growaux_(L, v, &(size), sizeof(t), limit, e));               \
    }                                                                          \
  } while (false)

#define luaM_reallocVector(L, v, oldSize, newSize, ty)                         \
  ((v) = luaM_reallocv(L, v, oldSize, newSize, sizeof(ty)))
