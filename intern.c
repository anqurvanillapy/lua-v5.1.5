#include <string.h>

#include "lua.h"

#include "intern.h"
#include "memory.h"
#include "object.h"
#include "state.h"

void StringPool_resize(lua_State *L, size_t newSize) {
  if (G(L)->gcstate == GCSsweepstring) {
    // Cannot resize during GC traversal.
    return;
  }

  GCObject **newHash = Mem_newVec(L, newSize, GCObject *);
  for (size_t i = 0; i < newSize; i++) {
    newHash[i] = nullptr;
  }

  // Rehash.
  StringPool *tb = &G(L)->pool;
  for (size_t i = 0; i < tb->bucketsSize; i++) {
    GCObject *p = tb->buckets[i];
    while (p) {
      // New bucket position.
      uint32_t h = gco2ts(p)->hash;
      size_t pos = lmod(h, newSize);
      assert((int)(h % newSize) == lmod(h, newSize));

      // Chain it.
      GCObject *next = p->gch.next;
      p->gch.next = newHash[pos];
      newHash[pos] = p;
      p = next;
    }
  }

  Mem_freeVec(L, tb->buckets, tb->bucketsSize, String *);
  tb->bucketsSize = (int)newSize;
  tb->buckets = newHash;
}

static String *createStr(lua_State *L, const char *str, size_t len,
                         uint32_t h) {
  if (len > (SIZE_MAX - sizeof(String)) / sizeof(char) - 1) {
    Mem_throwTooBig(L);
  }

  String *ts = Mem_alloc(L, sizeof(String) + len + 1);
  ts->len = len;
  ts->hash = h;
  ts->header.marked = luaC_white(G(L));
  ts->header.tt = LUA_TYPE_STRING;
  ts->keywordID = 0;
  memcpy(ts + 1, str, len * sizeof(char));
  ((char *)(ts + 1))[len] = '\0';

  StringPool *pool = &G(L)->pool;
  h = lmod(h, pool->bucketsSize);
  // Chain the new entry.
  ts->header.next = pool->buckets[h];
  pool->buckets[h] = LuaObjectToGCObject(ts);
  pool->itemsNum++;

  if (pool->itemsNum > (size_t)pool->bucketsSize &&
      pool->bucketsSize <= SAFE_INT_MAX / 2) {
    StringPool_resize(L, pool->bucketsSize * 2);
  }

  return ts;
}

String *String_createSized(lua_State *L, const char *str, size_t len) {
  // Seed.
  uint32_t h = (uint32_t)len;
  // If the string is too long, don't hash all of its characters.
  size_t step = (len >> 5) + 1;

  // Compute hash.
  for (size_t i = len; i >= step; i -= step) {
    h = h ^ ((h << 5) + (h >> 2) + (uint8_t)str[i - 1]);
  }

  // Iterate the string pool.
  for (GCObject *o = G(L)->pool.buckets[lmod(h, G(L)->pool.bucketsSize)];
       o != nullptr; o = o->gch.next) {
    String *ts = gco2ts(o);
    if (ts->len != len || memcmp(str, STRING_CONTENT(ts), len) != 0) {
      continue;
    }
    if (IS_DEAD(G(L), o)) {
      // This string may be dead, make it alive.
      changewhite(o);
    }
    // Return the existing one.
    return ts;
  }

  // No such string, create a new one.
  return createStr(L, str, len, h);
}

Userdata *Userdata_new(lua_State *L, size_t size, Table *env) {
  if (size > SIZE_MAX - sizeof(Userdata)) {
    Mem_throwTooBig(L);
  }

  Userdata *u = Mem_alloc(L, size + sizeof(Userdata));
  u->header.marked = luaC_white(G(L)); /* is not finalized */
  u->header.tt = LUA_TYPE_USERDATA;
  u->len = size;
  u->metatable = nullptr;
  u->env = env;
  /* chain it on udata list (after main thread) */
  u->header.next = G(L)->mainthread->header.next;
  G(L)->mainthread->header.next = LuaObjectToGCObject(u);
  return u;
}
