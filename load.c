#include "load.h"
#include "buffer.h"
#include "closure.h"
#include "debug.h"
#include "intern.h"
#include "memory.h"
#include "object.h"
#include "stack.h"

typedef struct {
  lua_State *L;
  ZIO *Z;
  StringBuilder *b;
  const char *name;
} LoadState;

static void error(LoadState *S, const char *why) {
  Object_sprintf(S->L, "%s: %s in precompiled chunk", S->name, why);
  Stack_throw(S->L, LUA_ERRSYNTAX);
}

#define LoadMem(S, b, n, size) LoadBlock(S, b, (n) * (size))
#define LoadByte(S) (uint8_t)LoadChar(S)
#define LoadVar(S, x) LoadMem(S, &x, 1, sizeof(x))
#define LoadVector(S, b, n, size) LoadMem(S, b, n, size)

static void LoadBlock(LoadState *S, void *b, size_t size) {
  size_t r = luaZ_read(S->Z, b, size);
  if (r != 0) {
    error(S, "unexpected end");
  }
}

static int LoadChar(LoadState *S) {
  char x;
  LoadVar(S, x);
  return x;
}

static int LoadInt(LoadState *S) {
  int x;
  LoadVar(S, x);
  if (x < 0) {
    error(S, "bad integer");
  }
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
  }
  char *s = luaZ_reserve(S->L, S->b, size);
  LoadBlock(S, s, size);
  return String_createSized(S->L, s, size - 1); /* remove trailing '\0' */
}

static void LoadCode(LoadState *S, Prototype *f) {
  int n = LoadInt(S);
  f->code = Mem_newVec(S->L, n, Instruction);
  f->codeSize = n;
  LoadVector(S, f->code, n, sizeof(Instruction));
}

static Prototype *LoadFunction(LoadState *S, String *p);

static void LoadConstants(LoadState *S, Prototype *f) {
  int i, n;
  n = LoadInt(S);
  f->constants = Mem_newVec(S->L, n, Value);
  f->constantsSize = n;
  for (i = 0; i < n; i++) {
    SET_NIL(&f->constants[i]);
  }
  for (i = 0; i < n; i++) {
    Value *o = &f->constants[i];
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
  f->inners = Mem_newVec(S->L, n, Prototype *);
  f->innersSize = n;
  for (i = 0; i < n; i++) {
    f->inners[i] = nullptr;
  }
  for (i = 0; i < n; i++) {
    f->inners[i] = LoadFunction(S, f->source);
  }
}

static void LoadDebug(LoadState *S, Prototype *f) {
  int i, n;
  n = LoadInt(S);
  f->lineInfo = Mem_newVec(S->L, n, int);
  f->lineInfoSize = n;
  LoadVector(S, f->lineInfo, n, sizeof(int));
  n = LoadInt(S);
  f->locVars = Mem_newVec(S->L, n, LocVar);
  f->locVarsSize = n;
  for (i = 0; i < n; i++) {
    f->locVars[i].name = nullptr;
  }
  for (i = 0; i < n; i++) {
    f->locVars[i].name = LoadString(S);
    f->locVars[i].startPC = LoadInt(S);
    f->locVars[i].endPC = LoadInt(S);
  }
  n = LoadInt(S);
  f->upvalues = Mem_newVec(S->L, n, String *);
  f->upvaluesSize = n;
  for (i = 0; i < n; i++) {
    f->upvalues[i] = nullptr;
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
  f = Prototype_new(S->L);
  SET_PROTO_TO_STACK(S->L, S->L->top, f);
  incr_top(S->L);
  f->source = LoadString(S);
  if (f->source == nullptr) {
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
  if (!luaG_checkcode(f)) {
    error(S, "bad code");
  }
  S->L->top--;
  S->L->nestedCCallsNum--;
  return f;
}

static void LoadHeader(LoadState *S) {
  char h[HEADER_SIZE];
  char s[HEADER_SIZE];
  luaU_header(h);
  LoadBlock(S, s, HEADER_SIZE);
  if (memcmp(h, s, 12) != 0) {
    error(S, "bad header");
  }
}

/*
** load precompiled chunk
*/
Prototype *luaU_undump(lua_State *L, ZIO *Z, StringBuilder *buff,
                       const char *name) {
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
  *h++ = (char)HEADER_VERSION;
  *h++ = (char)HEADER_FORMAT;
  *h++ = (char)*(char *)&x; /* endianness */
  *h++ = (char)sizeof(int);
  *h++ = (char)sizeof(size_t);
  *h++ = (char)sizeof(Instruction);
  *h++ = (char)sizeof(double);
  // Is the number type integral, always false.
  *h = (char)false;
}
