/* String table (keeps all strings handled by Lua). */

#include <string.h>

#include "lua.h"

#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"

void luaS_resize(lua_State *L, int newsize) {
  GCObject **newhash;
  stringtable *tb;
  int i;
  if (G(L)->gcstate == GCSsweepstring) {
    return; /* cannot resize during GC traverse */
  }
  newhash = luaM_newvector(L, newsize, GCObject *);
  tb = &G(L)->strt;
  for (i = 0; i < newsize; i++) {
    newhash[i] = NULL;
  }
  /* rehash */
  for (i = 0; i < tb->size; i++) {
    GCObject *p = tb->hash[i];
    while (p) {                     /* for each node in the list */
      GCObject *next = p->gch.next; /* save next */
      unsigned int h = gco2ts(p)->hash;
      int h1 = lmod(h, newsize); /* new position */
      DEBUG_ASSERT(cast_int(h % newsize) == lmod(h, newsize));
      p->gch.next = newhash[h1]; /* chain it */
      newhash[h1] = p;
      p = next;
    }
  }
  luaM_freearray(L, tb->hash, tb->size, TString *);
  tb->size = newsize;
  tb->hash = newhash;
}

static TString *newlstr(lua_State *L, const char *str, size_t l,
                        unsigned int h) {
  TString *ts;
  stringtable *tb;
  if (l + 1 > (MAX_SIZET - sizeof(TString)) / sizeof(char)) {
    luaM_toobig(L);
  }
  ts =
      cast(TString *, luaM_malloc(L, (l + 1) * sizeof(char) + sizeof(TString)));
  ts->tsv.len = l;
  ts->tsv.hash = h;
  ts->tsv.marked = luaC_white(G(L));
  ts->tsv.tt = LUA_TYPE_STRING;
  ts->tsv.reserved = 0;
  memcpy(ts + 1, str, l * sizeof(char));
  ((char *)(ts + 1))[l] = '\0'; /* ending 0 */
  tb = &G(L)->strt;
  h = lmod(h, tb->size);
  ts->tsv.next = tb->hash[h]; /* chain new entry */
  tb->hash[h] = LuaObjectToGCObject(ts);
  tb->nuse++;
  if (tb->nuse > cast(lu_int32, tb->size) && tb->size <= MAX_INT / 2) {
    luaS_resize(L, tb->size * 2); /* too crowded */
  }
  return ts;
}

TString *luaS_newlstr(lua_State *L, const char *str, size_t l) {
  GCObject *o;
  unsigned int h = cast(unsigned int, l); /* seed */
  size_t step =
      (l >> 5) + 1; /* if string is too long, don't hash all its chars */
  size_t l1;
  for (l1 = l; l1 >= step; l1 -= step) { /* compute hash */
    h = h ^ ((h << 5) + (h >> 2) + cast(unsigned char, str[l1 - 1]));
  }
  for (o = G(L)->strt.hash[lmod(h, G(L)->strt.size)]; o != NULL;
       o = o->gch.next) {
    TString *ts = rawgco2ts(o);
    if (ts->tsv.len == l && (memcmp(str, GET_STR(ts), l) == 0)) {
      /* string may be dead */
      if (IS_DEAD(G(L), o)) {
        changewhite(o);
      }
      return ts;
    }
  }
  return newlstr(L, str, l, h); /* not found */
}

Userdata *luaS_newudata(lua_State *L, size_t s, Table *e) {
  Userdata *u;
  if (s > MAX_SIZET - sizeof(Userdata)) {
    luaM_toobig(L);
  }
  u = cast(Userdata *, luaM_malloc(L, s + sizeof(Userdata)));
  u->uv.marked = luaC_white(G(L)); /* is not finalized */
  u->uv.tt = LUA_TYPE_USERDATA;
  u->uv.len = s;
  u->uv.metatable = NULL;
  u->uv.env = e;
  /* chain it on udata list (after main thread) */
  u->uv.next = G(L)->mainthread->next;
  G(L)->mainthread->next = LuaObjectToGCObject(u);
  return u;
}
