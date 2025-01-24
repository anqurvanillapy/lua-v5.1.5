// Buffered reader.

#pragma once

#include "memory.h"

typedef struct StringBuilder {
  char *str;
  size_t size;
  size_t len;
} StringBuilder;

#define StringBuilder_init(L, b)                                               \
  do {                                                                         \
    (b)->str = nullptr;                                                        \
    (b)->size = 0;                                                             \
    (b)->len = 0;                                                              \
  } while (false)
#define StringBuilder_get(b) ((b)->str)
#define StringBuilder_len(b) ((b)->len)
#define StringBuilder_size(b) ((b)->size)
#define StringBuilder_reset(b)                                                 \
  do {                                                                         \
    (b)->len = 0;                                                              \
  } while (false)
#define StringBuilder_resize(L, b, sz)                                         \
  do {                                                                         \
    Mem_reallocVec(L, (b)->str, (b)->size, sz, char);                          \
    (b)->size = (sz);                                                          \
  } while (false)
#define StringBuilder_free(L, b) StringBuilder_resize(L, b, 0)

#define EOZ (-1) /* end of stream */
#define READ_CHAR(z) (((z)->n--) > 0 ? (int)(uint8_t)*(z)->p++ : luaZ_fill(z))

typedef struct Zio {
  // Number of bytes still unread.
  size_t n;
  // Current position in the buffer.
  const char *p;

  lua_Reader reader;
  void *ud;
  lua_State *L;
} ZIO;

LUAI_FUNC char *luaZ_reserve(lua_State *L, StringBuilder *buff, size_t n);
LUAI_FUNC void luaZ_init(lua_State *L, ZIO *z, lua_Reader reader, void *ud);
// Read next n bytes.
LUAI_FUNC size_t luaZ_read(ZIO *z, void *b, size_t n);
LUAI_FUNC int luaZ_lookahead(ZIO *z);

LUAI_FUNC int luaZ_fill(ZIO *z);
