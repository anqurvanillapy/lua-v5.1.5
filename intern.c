#include <string.h>

#include "lua.h"

#include "intern.h"
#include "lmem.h"
#include "lstate.h"
#include "object.h"

void String_resize(lua_State *L, int newSize) {
  if (G(L)->gcstate == GCSsweepstring) {
    // Cannot resize during GC traverse.
    return;
  }

  GCObject **newHash = luaM_newvector(L, newSize, GCObject *);
  for (int i = 0; i < newSize; i++) {
    newHash[i] = nullptr;
  }

  // Rehash.
  StringPool *tb = &G(L)->pool;
  for (int i = 0; i < tb->bucketsSize; i++) {
    GCObject *p = tb->buckets[i];
    while (p) {                     /* for each node in the list */
      GCObject *next = p->gch.next; /* save next */
      uint32_t h = gco2ts(p)->hash;
      int h1 = lmod(h, newSize); /* new position */
      DEBUG_ASSERT(cast_int(h % newSize) == lmod(h, newSize));
      p->gch.next = newHash[h1]; /* chain it */
      newHash[h1] = p;
      p = next;
    }
  }

  luaM_freearray(L, tb->buckets, tb->bucketsSize, String *);
  tb->bucketsSize = newSize;
  tb->buckets = newHash;
}

static String *createStr(lua_State *L, const char *str, size_t l, uint32_t h) {
  if (l > (SIZE_MAX - sizeof(String)) / sizeof(char) - 1) {
    luaM_toobig(L);
  }

  String *ts = luaM_malloc(L, (l + 1) * sizeof(char) + sizeof(String));
  ts->len = l;
  ts->hash = h;
  ts->header.marked = luaC_white(G(L));
  ts->header.tt = LUA_TYPE_STRING;
  ts->keywordID = 0;
  memcpy(ts + 1, str, l * sizeof(char));
  ((char *)(ts + 1))[l] = '\0'; /* ending 0 */

  StringPool *tb = &G(L)->pool;
  h = lmod(h, tb->bucketsSize);
  ts->header.next = tb->buckets[h]; /* chain new entry */
  tb->buckets[h] = LuaObjectToGCObject(ts);
  tb->itemsNum++;

  if (tb->itemsNum > cast(lu_int32, tb->bucketsSize) &&
      tb->bucketsSize <= MAX_INT / 2) {
    String_resize(L, tb->bucketsSize * 2); /* too crowded */
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
    luaM_toobig(L);
  }

  Userdata *u = luaM_malloc(L, size + sizeof(Userdata));
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
