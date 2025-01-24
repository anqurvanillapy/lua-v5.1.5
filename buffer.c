#include <string.h>

#include "buffer.h"
#include "limits.h"
#include "memory.h"
#include "state.h"

int luaZ_fill(ZIO *z) {
  lua_State *L = z->L;
  lua_unlock(L);
  // Enter the userspace, so we unlock first.
  size_t size;
  const char *buff = z->reader(L, z->ud, &size);
  lua_lock(L);
  if (buff == nullptr || size == 0) {
    return EOZ;
  }
  z->n = size - 1;
  z->p = buff;
  return (int)(uint8_t)*z->p++;
}

int luaZ_lookahead(ZIO *z) {
  if (z->n == 0) {
    if (luaZ_fill(z) == EOZ) {
      return EOZ;
    }
    // luaZ_fill removed first byte, put it back.
    z->n++;
    z->p--;
  }
  return (int)(uint8_t)*z->p;
}

void luaZ_init(lua_State *L, ZIO *z, lua_Reader reader, void *ud) {
  z->L = L;
  z->reader = reader;
  z->ud = ud;
  z->n = 0;
  z->p = nullptr;
}

size_t luaZ_read(ZIO *z, void *b, size_t n) {
  uint8_t *bytes = b;
  while (n) {
    if (luaZ_lookahead(z) == EOZ) {
      // Return number of missing bytes.
      return n;
    }
    size_t m = n <= z->n ? n : z->n; /* min. between n and z->n */
    memcpy(bytes, z->p, m);
    z->n -= m;
    z->p += m;
    bytes += m;
    n -= m;
  }
  return 0;
}

char *luaZ_reserve(lua_State *L, StringBuilder *buff, size_t n) {
  if (n > buff->size) {
    if (n < LUA_MIN_BUF_SIZE) {
      n = LUA_MIN_BUF_SIZE;
    }
    StringBuilder_resize(L, buff, n);
  }
  return buff->str;
}
