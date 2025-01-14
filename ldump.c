/* Save precompiled Lua chunks. */

#include <stddef.h>

#include "lua.h"

#include "lobject.h"
#include "lstate.h"
#include "lundump.h"

typedef struct {
  lua_State *L;
  lua_Writer writer;
  void *data;
  int strip;
  int status;
} DumpState;

#define DumpMem(b, n, size, D) DumpBlock(b, (n) * (size), D)
#define DumpVar(x, D) DumpMem(&x, 1, sizeof(x), D)

static void DumpBlock(const void *b, size_t size, DumpState *D) {
  if (D->status == 0) {
    lua_unlock(D->L);
    D->status = (*D->writer)(D->L, b, size, D->data);
    lua_lock(D->L);
  }
}

static void DumpChar(int y, DumpState *D) {
  char x = (char)y;
  DumpVar(x, D);
}

static void DumpInt(int x, DumpState *D) { DumpVar(x, D); }

static void DumpNumber(lua_Number x, DumpState *D) { DumpVar(x, D); }

static void DumpVector(const void *b, int n, size_t size, DumpState *D) {
  DumpInt(n, D);
  DumpMem(b, n, size, D);
}

static void DumpString(const TString *s, DumpState *D) {
  if (s == NULL || GET_STR(s) == NULL) {
    size_t size = 0;
    DumpVar(size, D);
  } else {
    size_t size = s->tsv.len + 1; /* include trailing '\0' */
    DumpVar(size, D);
    DumpBlock(GET_STR(s), size, D);
  }
}

#define DumpCode(f, D) DumpVector(f->code, f->codeSize, sizeof(Instruction), D)

static void DumpFunction(const Prototype *f, const TString *p, DumpState *D);

static void DumpConstants(const Prototype *f, DumpState *D) {
  int i, n = f->kSize;
  DumpInt(n, D);
  for (i = 0; i < n; i++) {
    const TaggedValue *o = &f->k[i];
    DumpChar(GET_TYPE(o), D);
    switch (GET_TYPE(o)) {
    case LUA_TYPE_NIL:
      break;
    case LUA_TYPE_BOOLEAN:
      DumpChar(BOOL_VALUE(o), D);
      break;
    case LUA_TYPE_NUMBER:
      DumpNumber(NUMBER_VALUE(o), D);
      break;
    case LUA_TYPE_STRING:
      DumpString(RAW_STRING_VALUE(o), D);
      break;
    default:
      lua_assert(0); /* cannot happen */
      break;
    }
  }
  n = f->pSize;
  DumpInt(n, D);
  for (i = 0; i < n; i++) {
    DumpFunction(f->inners[i], f->source, D);
  }
}

static void DumpDebug(const Prototype *f, DumpState *D) {
  int i, n;
  n = (D->strip) ? 0 : f->lineInfoSize;
  DumpVector(f->lineInfo, n, sizeof(int), D);
  n = (D->strip) ? 0 : f->locVarsSize;
  DumpInt(n, D);
  for (i = 0; i < n; i++) {
    DumpString(f->locVars[i].varname, D);
    DumpInt(f->locVars[i].startPC, D);
    DumpInt(f->locVars[i].endPC, D);
  }
  n = (D->strip) ? 0 : f->upvaluesSize;
  DumpInt(n, D);
  for (i = 0; i < n; i++) {
    DumpString(f->upvalues[i], D);
  }
}

static void DumpFunction(const Prototype *f, const TString *p, DumpState *D) {
  DumpString((f->source == p || D->strip) ? NULL : f->source, D);
  DumpInt(f->lineDefined, D);
  DumpInt(f->lineDefinedLast, D);
  DumpChar(f->upvalueNum, D);
  DumpChar(f->paramNum, D);
  DumpChar(f->varargMode, D);
  DumpChar(f->maxStackSize, D);
  DumpCode(f, D);
  DumpConstants(f, D);
  DumpDebug(f, D);
}

static void DumpHeader(DumpState *D) {
  char h[LUAC_HEADERSIZE];
  luaU_header(h);
  DumpBlock(h, LUAC_HEADERSIZE, D);
}

/*
** dump Lua function as precompiled chunk
*/
int luaU_dump(lua_State *L, const Prototype *f, lua_Writer w, void *data,
              int strip) {
  DumpState D;
  D.L = L;
  D.writer = w;
  D.data = data;
  D.strip = strip;
  D.status = 0;
  DumpHeader(&D);
  DumpFunction(f, NULL, &D);
  return D.status;
}
