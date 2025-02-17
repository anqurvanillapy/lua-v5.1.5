#include <stddef.h>

#include "debug.h"
#include "memory.h"
#include "stack.h"
#include "state.h"

#define ARRAY_MIN_SIZE 4

void *Mem_doGrowVec(lua_State *L, void *block, size_t *size, size_t elemSize,
                    size_t limit, const char *errMsg) {
  // Grow to the limit when memory almost runs out.
  size_t newSize = limit;
  if (*size >= limit / 2 && *size >= limit) {
    // Cannot double it and even grow a little.
    luaG_runerror(L, errMsg);
  } else {
    // Most of the time, we double the capacity.
    newSize = (*size) * 2;
    if (newSize < ARRAY_MIN_SIZE) {
      newSize = ARRAY_MIN_SIZE;
    }
  }
  void *newBlock = Mem_reallocCnt(L, block, *size, newSize, elemSize);
  // Update only when everything else is OK.
  *size = newSize;
  return newBlock;
}

void *Mem_throwTooBig(lua_State *L) {
  luaG_runerror(L, "memory allocation error: block too big");
  return nullptr;
}

void *Mem_doRealloc(lua_State *L, void *block, size_t oldSize, size_t newSize) {
  GlobalState *g = G(L);
  assert((oldSize == 0) == (block == nullptr));
  block = g->alloc(g->allocData, block, oldSize, newSize);
  if (block == nullptr && newSize > 0) {
    Stack_throw(L, LUA_ERRMEM);
  }
  assert((newSize == 0) == (block == nullptr));
  g->totalbytes += newSize - oldSize;
  return block;
}
