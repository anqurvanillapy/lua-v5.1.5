/* Load precompiled Lua chunks. */

#include <string.h>

#include "lua.h"

#include "closure.h"
#include "debug.h"
#include "intern.h"
#include "lundump.h"
#include "lzio.h"
#include "memory.h"
#include "object.h"
#include "stack.h"

typedef struct {
  lua_State *L;
  ZIO *Z;
  Mbuffer *b;
  const char *name;
} LoadState;

#ifdef LUAC_TRUST_BINARIES
#define IF(c, s)
#define error(S, s)
#else
#define IF(c, s)                                                               \
  if (c)                                                                       \
  error(S, s)

static void error(LoadState *S, const char *why) {
  luaO_pushfstring(S->L, "%s: %s in precompiled chunk", S->name, why);
  luaD_throw(S->L, LUA_ERRSYNTAX);
}
#endif

#define LoadMem(S, b, n, size) LoadBlock(S, b, (n) * (size))
#define LoadByte(S) (uint8_t)LoadChar(S)
#define LoadVar(S, x) LoadMem(S, &x, 1, sizeof(x))
#define LoadVector(S, b, n, size) LoadMem(S, b, n, size)

static void LoadBlock(LoadState *S, void *b, size_t size) {
  size_t r = luaZ_read(S->Z, b, size);
  IF(r != 0, "unexpected end");
}

static int LoadChar(LoadState *S) {
  char x;
  LoadVar(S, x);
  return x;
}

static int LoadInt(LoadState *S) {
  int x;
  LoadVar(S, x);
  IF(x < 0, "bad integer");
  return x;
}

static double LoadNumber(LoadState *S) {
  double x;
  LoadVar(S, x);
  return x;
}

static String *LoadString(LoadState *S) {
  size_t size;
  LoadVar(S, size);
  if (size == 0) {
    return nullptr;
  } else {
    char *s = luaZ_openspace(S->L, S->b, size);
    LoadBlock(S, s, size);
    return String_createSized(S->L, s, size - 1); /* remove trailing '\0' */
  }
}

static void LoadCode(LoadState *S, Prototype *f) {
  int n = LoadInt(S);
  f->code = luaM_newvector(S->L, n, Instruction);
  f->codeSize = n;
  LoadVector(S, f->code, n, sizeof(Instruction));
}

static Prototype *LoadFunction(LoadState *S, String *p);

static void LoadConstants(LoadState *S, Prototype *f) {
  int i, n;
  n = LoadInt(S);
  f->k = luaM_newvector(S->L, n, Value);
  f->kSize = n;
  for (i = 0; i < n; i++) {
    SET_NIL(&f->k[i]);
  }
  for (i = 0; i < n; i++) {
    Value *o = &f->k[i];
    int t = LoadChar(S);
    switch (t) {
    case LUA_TYPE_NIL:
      SET_NIL(o);
      break;
    case LUA_TYPE_BOOLEAN:
      SET_BOOL(o, LoadChar(S) != 0);
      break;
    case LUA_TYPE_NUMBER:
      SET_NUMBER(o, LoadNumber(S));
      break;
    case LUA_TYPE_STRING:
      SET_STRING_TO_NEW(S->L, o, LoadString(S));
      break;
    default:
      error(S, "bad constant");
      break;
    }
  }
  n = LoadInt(S);
  f->inners = luaM_newvector(S->L, n, Prototype *);
  f->pSize = n;
  for (i = 0; i < n; i++) {
    f->inners[i] = NULL;
  }
  for (i = 0; i < n; i++) {
    f->inners[i] = LoadFunction(S, f->source);
  }
}

static void LoadDebug(LoadState *S, Prototype *f) {
  int i, n;
  n = LoadInt(S);
  f->lineInfo = luaM_newvector(S->L, n, int);
  f->lineInfoSize = n;
  LoadVector(S, f->lineInfo, n, sizeof(int));
  n = LoadInt(S);
  f->locVars = luaM_newvector(S->L, n, LocVar);
  f->locVarsSize = n;
  for (i = 0; i < n; i++) {
    f->locVars[i].varname = NULL;
  }
  for (i = 0; i < n; i++) {
    f->locVars[i].varname = LoadString(S);
    f->locVars[i].startPC = LoadInt(S);
    f->locVars[i].endPC = LoadInt(S);
  }
  n = LoadInt(S);
  f->upvalues = luaM_newvector(S->L, n, String *);
  f->upvaluesSize = n;
  for (i = 0; i < n; i++) {
    f->upvalues[i] = NULL;
  }
  for (i = 0; i < n; i++) {
    f->upvalues[i] = LoadString(S);
  }
}

static Prototype *LoadFunction(LoadState *S, String *p) {
  Prototype *f;
  if (++S->L->nestedCCallsNum > LUAI_MAX_C_CALLS) {
    error(S, "code too deep");
  }
  f = luaF_newproto(S->L);
  SET_PROTO_TO_STACK(S->L, S->L->top, f);
  incr_top(S->L);
  f->source = LoadString(S);
  if (f->source == NULL) {
    f->source = p;
  }
  f->lineDefined = LoadInt(S);
  f->lineDefinedLast = LoadInt(S);
  f->upvaluesNum = LoadByte(S);
  f->paramsNum = LoadByte(S);
  f->varargMode = LoadByte(S);
  f->maxStackSize = LoadByte(S);
  LoadCode(S, f);
  LoadConstants(S, f);
  LoadDebug(S, f);
  IF(!luaG_checkcode(f), "bad code");
  S->L->top--;
  S->L->nestedCCallsNum--;
  return f;
}

static void LoadHeader(LoadState *S) {
  char h[LUAC_HEADERSIZE];
  char s[LUAC_HEADERSIZE];
  luaU_header(h);
  LoadBlock(S, s, LUAC_HEADERSIZE);
  IF(memcmp(h, s, LUAC_HEADERSIZE) != 0, "bad header");
}

/*
** load precompiled chunk
*/
Prototype *luaU_undump(lua_State *L, ZIO *Z, Mbuffer *buff, const char *name) {
  LoadState S;
  if (*name == '@' || *name == '=') {
    S.name = name + 1;
  } else if (*name == LUA_SIGNATURE[0]) {
    S.name = "binary string";
  } else {
    S.name = name;
  }
  S.L = L;
  S.Z = Z;
  S.b = buff;
  LoadHeader(&S);
  return LoadFunction(&S, String_createLiteral(L, "=?"));
}

/*
 * make header
 */
void luaU_header(char *h) {
  int x = 1;
  memcpy(h, LUA_SIGNATURE, sizeof(LUA_SIGNATURE) - 1);
  h += sizeof(LUA_SIGNATURE) - 1;
  *h++ = (char)LUAC_VERSION;
  *h++ = (char)LUAC_FORMAT;
  *h++ = (char)*(char *)&x; /* endianness */
  *h++ = (char)sizeof(int);
  *h++ = (char)sizeof(size_t);
  *h++ = (char)sizeof(Instruction);
  *h++ = (char)sizeof(double);
  // Is the number type integral, always false.
  *h = (char)false;
}
