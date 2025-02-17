#include <stdarg.h>
#include <stddef.h>
#include <string.h>

#include "closure.h"
#include "debug.h"
#include "gc.h"
#include "lexer.h"
#include "object.h"
#include "opcodes.h"
#include "stack.h"
#include "state.h"
#include "table.h"
#include "vm.h"

static const char *getfuncname(lua_State *L, CallInfo *ci, const char **name);

static int currentpc(lua_State *L, CallInfo *ci) {
  if (!IS_CU_LUA_STRICT(ci)) {
    return -1; /* function is not a Lua function? */
  }
  if (ci == L->ci) {
    ci->savedpc = L->savedPC;
  }
  return pcRel(ci->savedpc, ci_func(ci)->l.p);
}

static int currentline(lua_State *L, CallInfo *ci) {
  int pc = currentpc(L, ci);
  if (pc < 0) {
    return -1; /* only active lua functions have current-line information */
  }
  return getline(ci_func(ci)->l.p, pc);
}

/*
** this function can be called asynchronous (e.g. during a signal)
*/
LUA_API int lua_sethook(lua_State *L, lua_Hook func, int mask, int count) {
  if (func == NULL || mask == 0) { /* turn off hooks? */
    mask = 0;
    func = nullptr;
  }
  L->hook = func;
  L->baseHookCount = count;
  RESET_HOOK_COUNT(L);
  L->hookMask = (uint8_t)mask;
  return 1;
}

LUA_API lua_Hook lua_gethook(lua_State *L) { return L->hook; }

LUA_API int lua_gethookmask(lua_State *L) { return L->hookMask; }

LUA_API int lua_gethookcount(lua_State *L) { return L->baseHookCount; }

LUA_API int lua_getstack(lua_State *L, int level, lua_Debug *ar) {
  int status;
  CallInfo *ci;
  lua_lock(L);
  for (ci = L->ci; level > 0 && ci > L->baseCI; ci--) {
    level--;
    if (IS_CI_LUA(ci)) {      /* Lua function? */
      level -= ci->tailcalls; /* skip lost tail calls */
    }
  }
  if (level == 0 && ci > L->baseCI) { /* level found? */
    status = 1;
    ar->i_ci = (int)(ci - L->baseCI);
  } else if (level < 0) { /* level is of a lost tail call? */
    status = 1;
    ar->i_ci = 0;
  } else {
    status = 0; /* no such level */
  }
  lua_unlock(L);
  return status;
}

static Prototype *getluaproto(CallInfo *ci) {
  return IS_CU_LUA_STRICT(ci) ? ci_func(ci)->l.p : nullptr;
}

static const char *findlocal(lua_State *L, CallInfo *ci, int n) {
  const char *name;
  Prototype *fp = getluaproto(ci);
  if (fp && (name = Prototype_getLocalName(fp, n, currentpc(L, ci))) != NULL) {
    return name; /* is a local variable in a Lua function */
  }
  StackIndex limit = (ci == L->ci) ? L->top : (ci + 1)->func;
  if (limit - ci->base >= n && n > 0) { /* is 'n' inside 'ci' stack? */
    return "(*temporary)";
  }
  return nullptr;
}

LUA_API const char *lua_getlocal(lua_State *L, const lua_Debug *ar, int n) {
  CallInfo *ci = L->baseCI + ar->i_ci;
  const char *name = findlocal(L, ci, n);
  lua_lock(L);
  if (name) {
    luaA_pushobject(L, ci->base + (n - 1));
  }
  lua_unlock(L);
  return name;
}

LUA_API const char *lua_setlocal(lua_State *L, const lua_Debug *ar, int n) {
  CallInfo *ci = L->baseCI + ar->i_ci;
  const char *name = findlocal(L, ci, n);
  lua_lock(L);
  if (name)
    SET_OBJECT_TO_SAME_STACK(L, ci->base + (n - 1), L->top - 1);
  L->top--; /* pop value */
  lua_unlock(L);
  return name;
}

static void funcinfo(lua_Debug *ar, Closure *cl) {
  if (cl->c.header.isC) {
    ar->source = "=[C]";
    ar->linedefined = -1;
    ar->lastlinedefined = -1;
    ar->what = "C";
  } else {
    ar->source = STRING_CONTENT(cl->l.p->source);
    ar->linedefined = cl->l.p->lineDefined;
    ar->lastlinedefined = cl->l.p->lineDefinedLast;
    ar->what = (ar->linedefined == 0) ? "main" : "Lua";
  }
  Lexer_chunkID(ar->short_src, ar->source, LUA_IDSIZE);
}

static void info_tailcall(lua_Debug *ar) {
  ar->name = ar->namewhat = "";
  ar->what = "tail";
  ar->lastlinedefined = -1;
  ar->linedefined = -1;
  ar->currentline = -1;
  ar->source = "=(tail call)";
  Lexer_chunkID(ar->short_src, ar->source, LUA_IDSIZE);
  ar->nups = 0;
}

static void collectValidLines(lua_State *L, Closure *f) {
  if (f == nullptr || f->c.header.isC) {
    SET_NIL(L->top);
  } else {
    Table *t = Table_new(L, 0, 0);
    int *lineInfo = f->l.p->lineInfo;
    for (size_t i = 0; i < f->l.p->lineInfoSize; i++) {
      SET_BOOL(Table_insertInteger(L, t, lineInfo[i]), 1);
    }
    SET_TABLE(L, L->top, t);
  }
  incr_top(L);
}

static int auxgetinfo(lua_State *L, const char *what, lua_Debug *ar, Closure *f,
                      CallInfo *ci) {
  int status = 1;
  if (f == NULL) {
    info_tailcall(ar);
    return status;
  }
  for (; *what; what++) {
    switch (*what) {
    case 'S': {
      funcinfo(ar, f);
      break;
    }
    case 'l': {
      ar->currentline = ci ? currentline(L, ci) : -1;
      break;
    }
    case 'u': {
      // FIXME(anqur): Suspicious conversion.
      ar->nups = (int)f->c.header.nupvalues;
      break;
    }
    case 'n': {
      ar->namewhat = ci ? getfuncname(L, ci, &ar->name) : nullptr;
      if (ar->namewhat == nullptr) {
        ar->namewhat = ""; /* not found */
        ar->name = nullptr;
      }
      break;
    }
    case 'L':
    case 'f': /* handled by lua_getinfo */
      break;
    default:
      status = 0; /* invalid option */
    }
  }
  return status;
}

LUA_API int lua_getinfo(lua_State *L, const char *what, lua_Debug *ar) {
  int status;
  Closure *f = nullptr;
  CallInfo *ci = nullptr;
  lua_lock(L);
  if (*what == '>') {
    StackIndex func = L->top - 1;
    assert(IS_TYPE_FUNCTION(func));
    what++; /* skip the '>' */
    f = CLOSURE_VALUE(func);
    L->top--;                 /* pop function */
  } else if (ar->i_ci != 0) { /* no tail call? */
    ci = L->baseCI + ar->i_ci;
    assert(IS_TYPE_FUNCTION(ci->func));
    f = CLOSURE_VALUE(ci->func);
  }
  status = auxgetinfo(L, what, ar, f, ci);
  if (strchr(what, 'f')) {
    if (f == NULL) {
      SET_NIL(L->top);
    } else
      SET_CLOSURE(L, L->top, f);
    incr_top(L);
  }
  if (strchr(what, 'L')) {
    collectValidLines(L, f);
  }
  lua_unlock(L);
  return status;
}

/*
** {======================================================
** Symbolic Execution and code checker
** =======================================================
*/

#define check(x)                                                               \
  do {                                                                         \
    if (!(x)) {                                                                \
      return 0;                                                                \
    }                                                                          \
  } while (false)

#define checkjump(pt, pc) check(0 <= pc && pc < pt->codeSize)

#define checkreg(pt, reg) check((reg) < (pt)->maxStackSize)

static int precheck(const Prototype *pt) {
  check(pt->maxStackSize <= MAXSTACK);
  check(pt->paramsNum + (pt->varargMode & VARARG_HAS_ARG) <= pt->maxStackSize);
  check(!(pt->varargMode & VARARG_NEEDS_ARG) ||
        (pt->varargMode & VARARG_HAS_ARG));
  check(pt->upvaluesSize <= pt->upvaluesNum);
  check(pt->lineInfoSize == pt->codeSize || pt->lineInfoSize == 0);
  check(pt->codeSize > 0 &&
        GET_OPCODE(pt->code[pt->codeSize - 1]) == OP_RETURN);
  return 1;
}

#define checkopenop(pt, pc) luaG_checkopenop((pt)->code[(pc) + 1])

int luaG_checkopenop(Instruction i) {
  switch (GET_OPCODE(i)) {
  case OP_CALL:
  case OP_TAILCALL:
  case OP_RETURN:
  case OP_SETLIST: {
    check(GETARG_B(i) == 0);
    return 1;
  }
  default:
    return 0; /* invalid instruction after an open call */
  }
}

static int checkArgMode(const Prototype *pt, int r, enum OpArgMask mode) {
  switch (mode) {
  case OP_ARG_NOT_USED:
    check(r == 0);
    break;
  case OP_ARG_USED:
    break;
  case OP_ARG_REG_OR_OFFSET:
    checkreg(pt, r);
    break;
  case OP_ARG_CONST_OR_REG:
    check(ISK(r) ? INDEXK(r) < pt->constantsSize : r < pt->maxStackSize);
    break;
  }
  return 1;
}

static Instruction symbexec(const Prototype *pt, size_t lastpc, int reg) {
  // Stores position of last instruction that changed register. Point to final
  // return (a neutral instruction).
  size_t last = pt->codeSize - 1;
  check(precheck(pt));
  for (size_t pc = 0; pc < lastpc; pc++) {
    Instruction i = pt->code[pc];
    OpCode op = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = 0;
    int c = 0;
    check(op < NUM_OPCODES);
    checkreg(pt, a);
    switch (GET_OP_MODE(op)) {
    case FORMAT_A_B_C: {
      b = GETARG_B(i);
      c = GETARG_C(i);
      check(checkArgMode(pt, b, GET_B_MODE(op)));
      check(checkArgMode(pt, c, GET_C_MODE(op)));
      break;
    }
    case FORMAT_A_Bx: {
      b = GETARG_Bx(i);
      if (GET_B_MODE(op) == OP_ARG_CONST_OR_REG) {
        check((size_t)b < pt->constantsSize);
      }
      break;
    }
    case FORMAT_A_sBx: {
      b = GETARG_sBx(i);
      if (GET_B_MODE(op) == OP_ARG_REG_OR_OFFSET) {
        size_t dest = pc + 1 + b;
        check(0 <= dest && dest < pt->codeSize);
        if (dest > 0) {
          /* check that it does not jump to a setlist count; this
             is tricky, because the count from a previous setlist may
             have the same value of an invalid setlist; so, we must
             go all the way back to the first of them (if any) */
          size_t j = 0;
          for (; j < dest; j++) {
            Instruction d = pt->code[dest - 1 - j];
            if (!(GET_OPCODE(d) == OP_SETLIST && GETARG_C(d) == 0)) {
              break;
            }
          }
          /* if 'j' is even, previous value is not a setlist (even if
             it looks like one) */
          check((j & 1) == 0);
        }
      }
      break;
    }
    }
    if (TEST_A_MODE(op)) {
      if (a == reg) {
        last = pc; /* change register `a' */
      }
    }
    if (TEST_T_MODE(op)) {
      check(pc + 2 < pt->codeSize); /* check skip */
      check(GET_OPCODE(pt->code[pc + 1]) == OP_JMP);
    }
    switch (op) {
    case OP_LOADBOOL: {
      /* does it jump? */
      if (c == 1) {
        check(pc + 2 < pt->codeSize); /* check its jump */
        check(GET_OPCODE(pt->code[pc + 1]) != OP_SETLIST ||
              GETARG_C(pt->code[pc + 1]) != 0);
      }
      break;
    }
    case OP_LOADNIL: {
      if (a <= reg && reg <= b) {
        last = pc; /* set registers from `a' to `b' */
      }
      break;
    }
    case OP_GETUPVAL:
    case OP_SETUPVAL: {
      check((size_t)b < pt->upvaluesNum);
      break;
    }
    case OP_GETGLOBAL:
    case OP_SETGLOBAL: {
      check(IS_TYPE_STRING(&pt->constants[b]));
      break;
    }
    case OP_SELF: {
      checkreg(pt, a + 1);
      if (reg == a + 1) {
        last = pc;
      }
      break;
    }
    case OP_CONCAT: {
      check(b < c); /* at least two operands */
      break;
    }
    case OP_TFORLOOP: {
      check(c >= 1);           /* at least one result (control variable) */
      checkreg(pt, a + 2 + c); /* space for results */
      if (reg >= a + 2) {
        last = pc; /* affect all regs above its base */
      }
      break;
    }
    case OP_FORLOOP:
    case OP_FORPREP:
      checkreg(pt, a + 3);
      /* go through */
    case OP_JMP: {
      size_t dest = pc + 1 + b;
      /* not full check and jump is forward and do not skip `lastpc'? */
      if (reg != NO_REG && pc < dest && dest <= lastpc) {
        pc += b; /* do the jump */
      }
      break;
    }
    case OP_CALL:
    case OP_TAILCALL: {
      if (b != 0) {
        checkreg(pt, a + b - 1);
      }
      c--; /* c = num. returns */
      if (c == LUA_MULTRET) {
        check(checkopenop(pt, pc));
      } else if (c != 0) {
        checkreg(pt, a + c - 1);
      }
      if (reg >= a) {
        last = pc; /* affect all registers above base */
      }
      break;
    }
    case OP_RETURN: {
      b--; /* b = num. returns */
      if (b > 0) {
        checkreg(pt, a + b - 1);
      }
      break;
    }
    case OP_SETLIST: {
      if (b > 0) {
        checkreg(pt, a + b);
      }
      if (c == 0) {
        pc++;
        check(pc < pt->codeSize - 1);
      }
      break;
    }
    case OP_CLOSURE: {
      check((size_t)b < pt->innersSize);
      size_t nup = pt->inners[b]->upvaluesNum;
      check(pc + nup < pt->codeSize);
      for (size_t j = 1; j <= nup; j++) {
        OpCode op1 = GET_OPCODE(pt->code[pc + j]);
        check(op1 == OP_GETUPVAL || op1 == OP_MOVE);
      }
      if (reg != NO_REG) { /* tracing? */
        pc += nup;         /* do not 'execute' these pseudo-instructions */
      }
      break;
    }
    case OP_VARARG: {
      check((pt->varargMode & VARARG_IS_VARARG) &&
            !(pt->varargMode & VARARG_NEEDS_ARG));
      b--;
      if (b == LUA_MULTRET) {
        check(checkopenop(pt, pc));
      }
      checkreg(pt, a + b - 1);
      break;
    }
    default:
      break;
    }
  }
  return pt->code[last];
}

#undef check
#undef checkjump
#undef checkreg

/* }====================================================== */

int luaG_checkcode(const Prototype *pt) {
  return (symbexec(pt, pt->codeSize, NO_REG) != 0);
}

static const char *kname(Prototype *p, int c) {
  if (ISK(c) && IS_TYPE_STRING(&p->constants[INDEXK(c)])) {
    return VALUE_STRING_CONTENT(&p->constants[INDEXK(c)]);
  } else {
    return "?";
  }
}

static const char *getobjname(lua_State *L, CallInfo *ci, int stackpos,
                              const char **name) {
  if (IS_CU_LUA_STRICT(ci)) { /* a Lua function? */
    Prototype *p = ci_func(ci)->l.p;
    int pc = currentpc(L, ci);
    Instruction i;
    *name = Prototype_getLocalName(p, stackpos + 1, pc);
    if (*name) { /* is a local? */
      return "local";
    }
    i = symbexec(p, pc, stackpos); /* try symbolic execution */
    assert(pc != -1);
    switch (GET_OPCODE(i)) {
    case OP_GETGLOBAL: {
      int g = GETARG_Bx(i); /* global index */
      assert(IS_TYPE_STRING(&p->constants[g]));
      *name = VALUE_STRING_CONTENT(&p->constants[g]);
      return "global";
    }
    case OP_MOVE: {
      int a = GETARG_A(i);
      int b = GETARG_B(i); /* move from `b' to `a' */
      if (b < a) {
        return getobjname(L, ci, b, name); /* get name for `b' */
      }
      break;
    }
    case OP_GETTABLE: {
      int k = GETARG_C(i); /* key index */
      *name = kname(p, k);
      return "field";
    }
    case OP_GETUPVAL: {
      int u = GETARG_B(i); /* upvalue index */
      *name = p->upvalues ? STRING_CONTENT(p->upvalues[u]) : "?";
      return "upvalue";
    }
    case OP_SELF: {
      int k = GETARG_C(i); /* key index */
      *name = kname(p, k);
      return "method";
    }
    default:
      break;
    }
  }
  return nullptr; /* no useful name found */
}

static const char *getfuncname(lua_State *L, CallInfo *ci, const char **name) {
  Instruction i;
  if ((IS_CU_LUA_STRICT(ci) && ci->tailcalls > 0) ||
      !IS_CU_LUA_STRICT(ci - 1)) {
    return nullptr; /* calling function is not Lua (or is unknown) */
  }
  ci--; /* calling function */
  i = ci_func(ci)->l.p->code[currentpc(L, ci)];
  if (GET_OPCODE(i) == OP_CALL || GET_OPCODE(i) == OP_TAILCALL ||
      GET_OPCODE(i) == OP_TFORLOOP) {
    return getobjname(L, ci, GETARG_A(i), name);
  } else {
    return nullptr; /* no useful name can be found */
  }
}

/* only ANSI way to check whether a pointer points to an array */
static int isinstack(CallInfo *ci, const Value *o) {
  StackIndex p;
  for (p = ci->base; p < ci->top; p++) {
    if (o == p) {
      return 1;
    }
  }
  return 0;
}

void luaG_typeerror(lua_State *L, const Value *o, const char *op) {
  const char *name = nullptr;
  const char *t = Debug_typeNames[GET_TYPE(o)];
  const char *kind = (isinstack(L->ci, o))
                         ? getobjname(L, L->ci, (int)(o - L->base), &name)
                         : nullptr;
  if (kind) {
    luaG_runerror(L, "attempt to %s %s '%s' (a %s value)", op, kind, name, t);
  } else {
    luaG_runerror(L, "attempt to %s a %s value", op, t);
  }
}

void luaG_concaterror(lua_State *L, StackIndex p1, StackIndex p2) {
  if (IS_TYPE_STRING(p1) || IS_TYPE_NUMBER(p1)) {
    p1 = p2;
  }
  assert(!IS_TYPE_STRING(p1) && !IS_TYPE_NUMBER(p1));
  luaG_typeerror(L, p1, "concatenate");
}

void luaG_aritherror(lua_State *L, const Value *p1, const Value *p2) {
  Value temp;
  if (luaV_tonumber(p1, &temp) == NULL) {
    p2 = p1; /* first operand is wrong */
  }
  luaG_typeerror(L, p2, "perform arithmetic on");
}

[[noreturn]] void luaG_ordererror(lua_State *L, const Value *p1,
                                  const Value *p2) {
  const char *t1 = Debug_typeNames[GET_TYPE(p1)];
  const char *t2 = Debug_typeNames[GET_TYPE(p2)];
  if (t1[2] == t2[2]) {
    luaG_runerror(L, "attempt to compare two %s values", t1);
  } else {
    luaG_runerror(L, "attempt to compare %s with %s", t1, t2);
  }
}

static void addinfo(lua_State *L, const char *msg) {
  CallInfo *ci = L->ci;
  if (IS_CU_LUA_STRICT(ci)) { /* is Lua code? */
    int line = currentline(L, ci);
    char buff[LUA_IDSIZE]; /* add file:line information */
    Lexer_chunkID(buff, STRING_CONTENT(getluaproto(ci)->source), LUA_IDSIZE);
    Object_sprintf(L, "%s:%d: %s", buff, line, msg);
  }
}

[[noreturn]] void luaG_errormsg(lua_State *L) {
  if (L->errFunc != 0) { /* is there an error handling function? */
    StackIndex errfunc = RESTORE_STACK(L, L->errFunc);
    if (!IS_TYPE_FUNCTION(errfunc)) {
      Stack_throw(L, LUA_ERRERR);
    }
    SET_OBJECT_TO_SAME_STACK(L, L->top, L->top - 1);  /* move argument */
    SET_OBJECT_TO_SAME_STACK(L, L->top - 1, errfunc); /* push function */
    incr_top(L);
    luaD_call(L, L->top - 2, 1); /* call it */
  }
  Stack_throw(L, LUA_ERRRUN);
}

[[noreturn]] void luaG_runerror(lua_State *L, const char *fmt, ...) {
  va_list argp;
  va_start(argp, fmt);
  addinfo(L, Object_vsprintf(L, fmt, argp));
  va_end(argp);
  luaG_errormsg(L);
}

void luaA_pushobject(lua_State *L, const Value *o) {
  SET_OBJECT_TO_STACK(L, L->top, o);
  API_CHECK(L, L->top < L->ci->top);
  L->top++;
}

const char *const Debug_typeNames[] = {
    "nil",      "boolean",  "userdata", "number", "string", "table",
    "function", "userdata", "thread",   "proto",  "upval",
};
