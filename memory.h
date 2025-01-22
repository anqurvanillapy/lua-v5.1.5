// Memory management.

#pragma once

#include <stddef.h>

#include "limits.h"

#define MEMERRMSG "not enough memory"

LUAI_FUNC void *Mem_doRealloc(lua_State *L, void *block, size_t oldSize,
                              size_t newSize);
LUAI_FUNC void *Mem_throwTooBig(lua_State *L);
LUAI_FUNC void *Mem_doGrowVec(lua_State *L, void *block, size_t *size,
                              size_t elemSize, size_t limit,
                              const char *errMsg);

#define Mem_reallocCnt(L, p, oldCnt, newCnt, elemSize)                         \
  ((((size_t)newCnt) <= SIZE_MAX / (elemSize))                                 \
       ? Mem_doRealloc(L, p, (oldCnt) * (elemSize), (newCnt) * (elemSize))     \
       : Mem_throwTooBig(L))

#define Mem_free(L, p, size) Mem_doRealloc(L, p, size, 0)
#define Mem_freePtr(L, p) Mem_doRealloc(L, p, sizeof(*(p)), 0)
#define Mem_freeVec(L, p, elemsNum, ty)                                        \
  Mem_reallocCnt(L, p, elemsNum, 0, sizeof(ty))

#define Mem_alloc(L, size) Mem_doRealloc(L, nullptr, 0, (size))
#define Mem_new(L, ty) Mem_alloc(L, sizeof(ty))
#define Mem_newVec(L, elemsNum, ty)                                            \
  Mem_reallocCnt(L, nullptr, 0, elemsNum, sizeof(ty))

#define Mem_growVec(L, v, elemsNum, size, ty, limit, errMsg)                   \
  do {                                                                         \
    if ((elemsNum) + 1 > (size)) {                                             \
      ((v) = Mem_doGrowVec(L, v, &(size), sizeof(ty), limit, errMsg));         \
    }                                                                          \
  } while (false)

#define Mem_reallocVec(L, v, oldSize, newSize, ty)                             \
  ((v) = Mem_reallocCnt(L, v, oldSize, newSize, sizeof(ty)))
