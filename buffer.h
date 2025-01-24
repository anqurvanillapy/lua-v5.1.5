// Buffered reader.

#pragma once

#include "memory.h"

#define EOZ (-1) /* end of stream */

#define char2int(c) (int)(uint8_t)(c)

#define zgetc(z) (((z)->n--) > 0 ? char2int(*(z)->p++) : luaZ_fill(z))

typedef struct Mbuffer {
  char *buffer;
  size_t n;
  size_t buffsize;
} Mbuffer;

#define luaZ_initbuffer(L, buff)                                               \
  do {                                                                         \
    (buff)->buffer = nullptr;                                                  \
    (buff)->buffsize = 0;                                                      \
  } while (false)

#define luaZ_buffer(buff) ((buff)->buffer)
#define luaZ_sizebuffer(buff) ((buff)->buffsize)
#define luaZ_bufflen(buff) ((buff)->n)

#define luaZ_resetbuffer(buff) ((buff)->n = 0)

#define luaZ_resizebuffer(L, buff, size)                                       \
  do {                                                                         \
    Mem_reallocVec(L, (buff)->buffer, (buff)->buffsize, size, char);           \
    (buff)->buffsize = size;                                                   \
  } while (false)

#define luaZ_freebuffer(L, buff) luaZ_resizebuffer(L, buff, 0)

typedef struct Zio {
  // Number of bytes still unread.
  size_t n;
  // Current position in the buffer.
  const char *p;

  lua_Reader reader;
  void *data;
  lua_State *L;
} ZIO;

LUAI_FUNC char *luaZ_reserve(lua_State *L, Mbuffer *buff, size_t n);
LUAI_FUNC void luaZ_init(lua_State *L, ZIO *z, lua_Reader reader, void *data);
// Read next n bytes.
LUAI_FUNC size_t luaZ_read(ZIO *z, void *b, size_t n);
LUAI_FUNC int luaZ_lookahead(ZIO *z);

LUAI_FUNC int luaZ_fill(ZIO *z);
