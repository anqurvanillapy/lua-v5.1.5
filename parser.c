#include <stdio.h>
#include <string.h>

#include "closure.h"
#include "codegen.h"
#include "debug.h"
#include "intern.h"
#include "lexer.h"
#include "memory.h"
#include "object.h"
#include "opcodes.h"
#include "parser.h"
#include "stack.h"
#include "state.h"
#include "table.h"

#define HAS_MULTI_RETURN(k) ((k) == EXPR_CALL || (k) == EXPR_VARARG_CALL)

#define GET_LOCAL_VAR(fs, i) ((fs)->f->locVars[(fs)->actvar[i]])

#define CHECK_LIMIT(fs, v, l, m)                                               \
  do {                                                                         \
    if ((v) > (l)) {                                                           \
      errorlimit(fs, l, m);                                                    \
    }                                                                          \
  } while (false)

/*
** nodes for block list (list of active blocks)
*/
typedef struct Block {
  struct Block *prev;
  // List of jumps out of this loop.
  int breaklist;
  // Number of active locals outside the breakable structure.
  uint8_t nactvar;
  // True if some variables in the block are upvalues.
  bool hasUpvalues;
  // True if this block is a loop.
  bool isBreakable;
} Block;

static void chunk(LexState *ls);
static void expr(LexState *ls, ExprInfo *v);

static void anchor_token(LexState *ls) {
  if (ls->t.token == TK_NAME || ls->t.token == TK_STRING) {
    String *ts = ls->t.literal.str;
    luaX_newstring(ls, STRING_CONTENT(ts), ts->len);
  }
}

static void throwToken(LexState *ls, int token) {
  Lex_throw(ls,
            Object_sprintf(ls->L, "'%s' expected", Lex_tokenText(ls, token)));
}

static void errorlimit(FuncState *fs, int limit, const char *what) {
  const char *msg =
      fs->f->lineDefined == 0
          ? Object_sprintf(fs->L, "main function has more than %d %s", limit,
                           what)
          : Object_sprintf(fs->L, "function at line %d has more than %d %s",
                           fs->f->lineDefined, limit, what);
  Lex_throwWith(fs->ls, msg, 0);
}

static bool testNext(LexState *ls, int c) {
  if (ls->t.token == c) {
    Lexer_next(ls);
    return true;
  }
  return false;
}

static void check(LexState *ls, int c) {
  if (ls->t.token != c) {
    throwToken(ls, c);
  }
}

static void checkNext(LexState *ls, int c) {
  check(ls, c);
  Lexer_next(ls);
}

static void check_match(LexState *ls, int what, int who, int where) {
  if (!testNext(ls, what)) {
    if (where == ls->linenumber) {
      throwToken(ls, what);
    } else {
      Lex_throw(ls, Object_sprintf(ls->L,
                                   "'%s' expected (to close '%s' at line %d)",
                                   Lex_tokenText(ls, what),
                                   Lex_tokenText(ls, who), where));
    }
  }
}

static String *checkName(LexState *ls) {
  check(ls, TK_NAME);
  String *ts = ls->t.literal.str;
  Lexer_next(ls);
  return ts;
}

static void exprSetKind(ExprInfo *e, ExprKind k) {
  e->f = NO_JUMP;
  e->t = NO_JUMP;
  e->k = k;
}

static void numberLiteral(ExprInfo *e, double value) {
  exprSetKind(e, EXPR_CONST_NUM);
  e->u.numValue = value;
}

static void stringLiteral(LexState *ls, ExprInfo *e, String *s) {
  exprSetKind(e, EXPR_CONST_STR);
  e->u.constID = Codegen_addString(ls->fs, s);
}

static void checkname(LexState *ls, ExprInfo *e) {
  stringLiteral(ls, e, checkName(ls));
}

static size_t registerLocalVar(LexState *ls, String *varname) {
  FuncState *fs = ls->fs;
  Prototype *f = fs->f;
  size_t oldSize = f->locVarsSize;
  Mem_growVec(ls->L, f->locVars, fs->nlocvars, f->locVarsSize, LocVar, SHRT_MAX,
              "too many local variables");
  while (oldSize < f->locVarsSize) {
    f->locVars[oldSize++].name = nullptr;
  }
  f->locVars[fs->nlocvars].name = varname;
  luaC_objbarrier(ls->L, f, varname);
  return fs->nlocvars++;
}

#define new_localvarliteral(ls, v, n)                                          \
  new_localvar(ls, luaX_newstring(ls, "" v, (sizeof(v) / sizeof(char)) - 1), n)

static void new_localvar(LexState *ls, String *name, int n) {
  FuncState *fs = ls->fs;
  CHECK_LIMIT(fs, fs->nactvar + n + 1, LUAI_MAX_VARS, "local variables");
  // FIXME(anqur): Conversion? Really?
  fs->actvar[fs->nactvar + n] = (int)registerLocalVar(ls, name);
}

static void adjustlocalvars(LexState *ls, int nvars) {
  FuncState *fs = ls->fs;
  fs->nactvar = (uint8_t)(fs->nactvar + nvars);
  for (; nvars; nvars--) {
    GET_LOCAL_VAR(fs, fs->nactvar - nvars).startPC = fs->pc;
  }
}

static void removevars(LexState *ls, int tolevel) {
  FuncState *fs = ls->fs;
  while (fs->nactvar > tolevel) {
    GET_LOCAL_VAR(fs, --fs->nactvar).endPC = fs->pc;
  }
}

static UpvalueInfo exprToUpvalueInfo(const ExprInfo *e) {
  switch (e->k) {
  case EXPR_LOCAL:
    return (UpvalueInfo){
        .k = e->k,
        .v = (UpvalueVariant){.localReg = e->u.localReg},
    };
  case EXPR_UPVALUE:
    return (UpvalueInfo){
        .k = e->k,
        .v = (UpvalueVariant){.upvalueID = e->u.upvalueID},
    };
  default:
    assert(false);
  }
}

static bool upvalueEqual(const UpvalueInfo *lhs, const UpvalueInfo *rhs) {
  if (lhs->k == EXPR_LOCAL && rhs->k == EXPR_LOCAL &&
      lhs->v.localReg == rhs->v.localReg) {
    return true;
  }
  if (lhs->k == EXPR_UPVALUE && rhs->k == EXPR_UPVALUE &&
      lhs->v.upvalueID == rhs->v.upvalueID) {
    return true;
  }
  return false;
}

static size_t createUpvalue(FuncState *fs, String *name, const ExprInfo *v) {
  assert(v->k == EXPR_LOCAL || v->k == EXPR_UPVALUE);
  UpvalueInfo rhs = exprToUpvalueInfo(v);
  Prototype *f = fs->f;
  for (size_t i = 0; i < f->upvaluesNum; i++) {
    if (upvalueEqual(&fs->upvalues[i], &rhs)) {
      assert(f->upvalues[i] == name);
      return i;
    }
  }

  // Create a new upvalue.
  size_t oldSize = f->upvaluesSize;
  CHECK_LIMIT(fs, f->upvaluesNum + 1, LUAI_MAX_UPVALUES, "upvalues");
  Mem_growVec(fs->L, f->upvalues, f->upvaluesNum, f->upvaluesSize, String *,
              SAFE_INT_MAX, "");
  while (oldSize < f->upvaluesSize) {
    f->upvalues[oldSize++] = nullptr;
  }
  f->upvalues[f->upvaluesNum] = name;
  luaC_objbarrier(fs->L, f, name);
  fs->upvalues[f->upvaluesNum] = rhs;
  return f->upvaluesNum++;
}

static int lookupLocalVar(FuncState *fs, String *name) {
  for (int i = fs->nactvar - 1; i >= 0; i--) {
    if (name == GET_LOCAL_VAR(fs, i).name) {
      return i;
    }
  }
  return -1;
}

static void markUpvalue(FuncState *fs, int level) {
  Block *bl = fs->bl;
  while (bl && bl->nactvar > level) {
    bl = bl->prev;
  }
  if (bl) {
    bl->hasUpvalues = true;
  }
}

static int lookupVar(FuncState *fs, String *n, ExprInfo *var, bool isBaseLvl) {
  if (fs == nullptr) {
    // No more levels found, it's a new global variable.
    exprSetKind(var, EXPR_GLOBAL);
    return EXPR_GLOBAL;
  }
  int v = lookupLocalVar(fs, n);
  if (v >= 0) {
    exprSetKind(var, EXPR_LOCAL);
    var->u.localReg = v;
    if (!isBaseLvl) {
      // This local variable will be used as an upvalue since it escapes the
      // base level.
      markUpvalue(fs, v);
    }
    return EXPR_LOCAL;
  }
  // Not found at the current level, try the upper one.
  if (lookupVar(fs->prev, n, var, false) == EXPR_GLOBAL) {
    return EXPR_GLOBAL;
  }
  var->u.upvalueID = createUpvalue(fs, n, var);
  var->k = EXPR_UPVALUE;
  return EXPR_UPVALUE;
}

static void singleVar(LexState *ls, ExprInfo *var) {
  String *name = checkName(ls);
  if (lookupVar(ls->fs, name, var, true) == EXPR_GLOBAL) {
    var->u.globalID = Codegen_addString(ls->fs, name);
  }
}

static void adjust_assign(LexState *ls, int nvars, int nexps, ExprInfo *e) {
  FuncState *fs = ls->fs;
  int extra = nvars - nexps;
  if (HAS_MULTI_RETURN(e->k)) {
    extra++; /* includes call itself */
    if (extra < 0) {
      extra = 0;
    }
    // Last expression provides the difference.
    Codegen_setReturnMulti(fs, e, extra);
    if (extra > 1) {
      luaK_reserveRegs(fs, extra - 1);
    }
  } else {
    if (e->k != EXPR_VOID) {
      Codegen_exprToNextReg(fs, e); /* close last expression */
    }
    if (extra > 0) {
      int reg = fs->freereg;
      luaK_reserveRegs(fs, extra);
      Codegen_emitNil(fs, reg, extra);
    }
  }
}

static void enterLevel(LexState *ls) {
  ls->L->nestedCCallsNum++;
  if (ls->L->nestedCCallsNum > LUAI_MAX_C_CALLS) {
    Lex_throwWith(ls, "chunk has too many syntax levels", 0);
  }
}

#define leaveLevel(ls) ((ls)->L->nestedCCallsNum--)

static void enterBlock(FuncState *fs, Block *bl, bool isBreakable) {
  bl->breaklist = NO_JUMP;
  bl->isBreakable = isBreakable;
  bl->nactvar = fs->nactvar;
  bl->hasUpvalues = false;
  bl->prev = fs->bl;
  fs->bl = bl;
  assert(fs->freereg == fs->nactvar);
}

static void leaveBlock(FuncState *fs) {
  Block *bl = fs->bl;
  fs->bl = bl->prev;
  removevars(fs->ls, bl->nactvar);
  if (bl->hasUpvalues) {
    Codegen_emitABC(fs, OP_CLOSE, bl->nactvar, 0, 0);
  }
  /* a block either controls scope or breaks (never both) */
  assert(!bl->isBreakable || !bl->hasUpvalues);
  assert(bl->nactvar == fs->nactvar);
  fs->freereg = fs->nactvar; /* free registers */
  Codegen_patchTo(fs, bl->breaklist);
}

static void pushclosure(LexState *ls, FuncState *func, ExprInfo *v) {
  FuncState *fs = ls->fs;
  Prototype *f = fs->f;
  size_t oldSize = f->innersSize;
  Mem_growVec(ls->L, f->inners, fs->np, f->innersSize, Prototype *, MAXARG_Bx,
              "constant table overflow");
  while (oldSize < f->innersSize) {
    f->inners[oldSize++] = nullptr;
  }
  f->inners[fs->np++] = func->f;
  luaC_objbarrier(ls->L, f, func->f);
  exprSetKind(v, EXPR_RELOC);
  v->u.relocatePC = Codegen_emitABx(fs, OP_CLOSURE, 0, fs->np - 1);
  for (size_t i = 0; i < func->f->upvaluesNum; i++) {
    if (func->upvalues[i].k == EXPR_LOCAL) {
      Codegen_emitABC(fs, OP_MOVE, 0, func->upvalues[i].v.localReg, 0);
    } else {
      // FIXME(anqur): Suspicious conversion.
      Codegen_emitABC(fs, OP_GETUPVAL, 0, (int)func->upvalues[i].v.upvalueID,
                      0);
    }
  }
}

static void openFunc(LexState *ls, FuncState *fs) {
  lua_State *L = ls->L;
  Prototype *f = Prototype_new(L);
  fs->f = f;
  fs->prev = ls->fs; /* linked list of funcstates */
  fs->ls = ls;
  fs->L = L;
  ls->fs = fs;
  fs->pc = 0;
  fs->lasttarget = -1;
  fs->jpc = NO_JUMP;
  fs->freereg = 0;
  fs->nk = 0;
  fs->np = 0;
  fs->nlocvars = 0;
  fs->nactvar = 0;
  fs->bl = nullptr;
  f->source = ls->source;
  f->maxStackSize = 2; /* registers 0/1 are always valid */
  fs->h = Table_new(L, 0, 0);
  /* anchor table of constants and prototype (to avoid being collected) */
  SET_TABLE_TO_STACK(L, L->top, fs->h);
  incr_top(L);
  SET_PROTO_TO_STACK(L, L->top, f);
  incr_top(L);
}

static void closeFunc(LexState *ls) {
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  Prototype *f = fs->f;
  removevars(ls, 0);
  Codegen_return(fs, 0, 0); /* final return */
  Mem_reallocVec(L, f->code, f->codeSize, fs->pc, Instruction);
  f->codeSize = fs->pc;
  Mem_reallocVec(L, f->lineInfo, f->lineInfoSize, fs->pc, int);
  f->lineInfoSize = fs->pc;
  Mem_reallocVec(L, f->constants, f->constantsSize, fs->nk, Value);
  f->constantsSize = fs->nk;
  Mem_reallocVec(L, f->inners, f->innersSize, fs->np, Prototype *);
  f->innersSize = fs->np;
  Mem_reallocVec(L, f->locVars, f->locVarsSize, fs->nlocvars, LocVar);
  f->locVarsSize = fs->nlocvars;
  Mem_reallocVec(L, f->upvalues, f->upvaluesSize, f->upvaluesNum, String *);
  f->upvaluesSize = f->upvaluesNum;
  assert(luaG_checkcode(f));
  assert(fs->bl == nullptr);
  ls->fs = fs->prev;
  /* last token read was anchored in defunct function; must re-anchor it */
  anchor_token(ls);
  L->top -= 2; /* remove table and prototype from the stack */
}

Prototype *luaY_parser(lua_State *L, ZIO *z, StringBuilder *buff,
                       const char *name) {
  struct LexState lexstate;
  struct FuncState funcstate;
  lexstate.tokens = buff;
  Lexer_setInput(L, &lexstate, z, String_create(L, name));
  openFunc(&lexstate, &funcstate);
  funcstate.f->varargMode = VARARG_IS_VARARG; /* main func. is always vararg */
  Lexer_next(&lexstate);                      /* read first token */
  chunk(&lexstate);
  check(&lexstate, TK_EOS);
  closeFunc(&lexstate);
  assert(funcstate.prev == nullptr);
  assert(funcstate.f->upvaluesNum == 0);
  assert(lexstate.fs == nullptr);
  return funcstate.f;
}

static void field(LexState *ls, ExprInfo *v) {
  /* field -> ['.' | ':'] NAME */
  FuncState *fs = ls->fs;
  ExprInfo key;
  Codegen_exprToAnyReg(fs, v);
  Lexer_next(ls); /* skip the dot or colon */
  checkname(ls, &key);
  Codegen_indexed(fs, v, &key);
}

/// \code
/// index
///     : '[' expr ']'
///     ;
/// \endcode
static void indexing(LexState *ls, ExprInfo *v) {
  Lexer_next(ls); /* skip the '[' */
  expr(ls, v);
  Codegen_exprToValue(ls->fs, v);
  checkNext(ls, ']');
}

struct ConsControl {
  ExprInfo v;  /* last list item read */
  ExprInfo *t; /* table descriptor */
  int nh;      /* total number of `record' elements */
  int na;      /* total number of array elements */
  int tostore; /* number of array elements pending to be stored */
};

static void recfield(LexState *ls, struct ConsControl *cc) {
  /* recfield -> (NAME | `['exp1`]') = exp1 */
  FuncState *fs = ls->fs;
  int reg = ls->fs->freereg;
  ExprInfo key, val;
  int rkkey;
  if (ls->t.token == TK_NAME) {
    CHECK_LIMIT(fs, cc->nh, SAFE_INT_MAX, "items in a constructor");
    checkname(ls, &key);
  } else { /* ls->t.token == '[' */
    indexing(ls, &key);
  }
  cc->nh++;
  checkNext(ls, '=');
  rkkey = Codegen_exprToRK(fs, &key);
  expr(ls, &val);
  assert(cc->t->k == EXPR_NON_RELOC);
  Codegen_emitABC(fs, OP_SETTABLE, cc->t->u.nonRelocReg, rkkey,
                  Codegen_exprToRK(fs, &val));
  fs->freereg = reg; /* free registers */
}

static void closelistfield(FuncState *fs, struct ConsControl *cc) {
  if (cc->v.k == EXPR_VOID) {
    return; /* there is no list item */
  }
  Codegen_exprToNextReg(fs, &cc->v);
  cc->v.k = EXPR_VOID;
  if (cc->tostore == LFIELDS_PER_FLUSH) {
    assert(cc->t->k == EXPR_NON_RELOC);
    luaK_setlist(fs, cc->t->u.nonRelocReg, cc->na, cc->tostore); /* flush */
    cc->tostore = 0; /* no more items pending */
  }
}

static void lastlistfield(FuncState *fs, struct ConsControl *cc) {
  if (cc->tostore == 0) {
    return;
  }
  if (HAS_MULTI_RETURN(cc->v.k)) {
    Codegen_setReturnMulti(fs, &cc->v, LUA_MULTRET);
    // FIXME(anqur): Suspicious conversion.
    luaK_setlist(fs,
                 cc->v.k == EXPR_CALL ? (int)cc->t->u.callPC
                                      : (int)cc->t->u.varargCallPC,
                 cc->na, LUA_MULTRET);
    cc->na--; /* do not count last expression (unknown number of elements) */
  } else {
    if (cc->v.k != EXPR_VOID) {
      Codegen_exprToNextReg(fs, &cc->v);
    }
    assert(cc->t->k == EXPR_NON_RELOC);
    luaK_setlist(fs, cc->t->u.nonRelocReg, cc->na, cc->tostore);
  }
}

static void listfield(LexState *ls, struct ConsControl *cc) {
  expr(ls, &cc->v);
  CHECK_LIMIT(ls->fs, cc->na, SAFE_INT_MAX, "items in a constructor");
  cc->na++;
  cc->tostore++;
}

static void constructor(LexState *ls, ExprInfo *t) {
  /* constructor -> ?? */
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  size_t pc = Codegen_emitABC(fs, OP_NEWTABLE, 0, 0, 0);
  struct ConsControl cc;
  cc.na = cc.nh = cc.tostore = 0;
  cc.t = t;
  exprSetKind(t, EXPR_RELOC);
  t->u.relocatePC = pc;
  exprSetKind(&cc.v, EXPR_VOID);    /* no value (yet) */
  Codegen_exprToNextReg(ls->fs, t); /* fix it at stack top (for gc) */
  checkNext(ls, '{');
  do {
    assert(cc.v.k == EXPR_VOID || cc.tostore > 0);
    if (ls->t.token == '}') {
      break;
    }
    closelistfield(fs, &cc);
    switch (ls->t.token) {
    case TK_NAME: { /* may be listfields or recfields */
      Lexer_lookahead(ls);
      if (ls->lookahead.token != '=') { /* expression? */
        listfield(ls, &cc);
      } else {
        recfield(ls, &cc);
      }
      break;
    }
    case '[': { /* constructor_item -> recfield */
      recfield(ls, &cc);
      break;
    }
    default: { /* constructor_part -> listfield */
      listfield(ls, &cc);
      break;
    }
    }
  } while (testNext(ls, ',') || testNext(ls, ';'));
  check_match(ls, '}', '{', line);
  lastlistfield(fs, &cc);
  SETARG_B(fs->f->code[pc], luaO_int2fb(cc.na)); /* set initial array size */
  SETARG_C(fs->f->code[pc], luaO_int2fb(cc.nh)); /* set initial table size */
}

static void parlist(LexState *ls) {
  /* parlist -> [ param { `,' param } ] */
  FuncState *fs = ls->fs;
  Prototype *f = fs->f;
  int nparams = 0;
  f->varargMode = 0;
  if (ls->t.token != ')') { /* is `parlist' not empty? */
    do {
      switch (ls->t.token) {
      case TK_NAME: { /* param -> NAME */
        new_localvar(ls, checkName(ls), nparams++);
        break;
      }
      case TK_DOTS: { /* param -> `...' */
        Lexer_next(ls);

        // Compatible with the old-style variadic arguments: Use `arg` as the
        // default name.
        new_localvarliteral(ls, "arg", nparams++);
        f->varargMode = VARARG_HAS_ARG | VARARG_NEEDS_ARG;

        f->varargMode |= VARARG_IS_VARARG;
        break;
      }
      default:
        Lex_throw(ls, "<name> or '...' expected");
      }
    } while (!f->varargMode && testNext(ls, ','));
  }
  adjustlocalvars(ls, nparams);
  f->paramsNum = (uint8_t)(fs->nactvar - (f->varargMode & VARARG_HAS_ARG));
  luaK_reserveRegs(fs, fs->nactvar); /* reserve register for parameters */
}

/// \code
/// body
///     : '(' parlist ')' chunk END
///     ;
/// \endcode
static void body(LexState *ls, ExprInfo *e, bool hasSelf, int line) {
  FuncState fs;
  openFunc(ls, &fs);
  fs.f->lineDefined = line;
  checkNext(ls, '(');
  if (hasSelf) {
    new_localvarliteral(ls, "self", 0);
    adjustlocalvars(ls, 1);
  }
  parlist(ls);
  checkNext(ls, ')');
  chunk(ls);
  fs.f->lineDefinedLast = ls->linenumber;
  check_match(ls, TK_END, TK_FUNCTION, line);
  closeFunc(ls);
  pushclosure(ls, &fs, e);
}

/// \code
/// exprList1
///     : expr (',' expr)*
///     ;
/// \endcode
static int exprList1(LexState *ls, ExprInfo *v) {
  int exprNum = 1;
  expr(ls, v);
  while (testNext(ls, ',')) {
    Codegen_exprToNextReg(ls->fs, v);
    expr(ls, v);
    exprNum++;
  }
  return exprNum;
}

/// \code
/// arguments
///     : '(' exprList1? ')'
///     | constructor
///     | STRING
///     ;
/// \endcode
static void arguments(LexState *ls, ExprInfo *f) {
  FuncState *fs = ls->fs;
  ExprInfo args;
  int line = ls->linenumber;
  switch (ls->t.token) {
  case '(':
    if (line != ls->lastline) {
      Lex_throw(ls, "ambiguous syntax (function call x new stmt)");
    }
    Lexer_next(ls);
    if (ls->t.token == ')') { /* arg list is empty? */
      args.k = EXPR_VOID;
    } else {
      exprList1(ls, &args);
      Codegen_setReturnMulti(fs, &args, LUA_MULTRET);
    }
    check_match(ls, ')', '(', line);
    break;
  case '{':
    constructor(ls, &args);
    break;
  case TK_STRING:
    stringLiteral(ls, &args, ls->t.literal.str);
    Lexer_next(ls); /* must use `literal' before `next' */
    break;
  default:
    Lex_throw(ls, "function arguments expected");
    return;
  }
  assert(f->k == EXPR_NON_RELOC);
  int base = f->u.nonRelocReg; /* base register for call */
  int nparams = LUA_MULTRET;   /* open call */
  if (!HAS_MULTI_RETURN(args.k)) {
    if (args.k != EXPR_VOID) {
      Codegen_exprToNextReg(fs, &args); /* close last argument */
    }
    nparams = fs->freereg - (base + 1);
  }
  exprSetKind(f, EXPR_CALL);
  f->u.callPC = Codegen_emitABC(fs, OP_CALL, base, nparams + 1, 2);
  Codegen_fixLine(fs, line);
  fs->freereg = base + 1; /* call remove function and arguments and leaves
                             (unless changed) one result */
}

/// \code
/// prefixExpr
///     : NAME
///     | '(' expr ')'
///     ;
/// \endcode
static void prefixExpr(LexState *ls, ExprInfo *v) {
  switch (ls->t.token) {
  case '(':
    int line = ls->linenumber;
    Lexer_next(ls);
    expr(ls, v);
    check_match(ls, ')', '(', line);
    Codegen_releaseVars(ls->fs, v);
    return;
  case TK_NAME:
    singleVar(ls, v);
    return;
  default:
    Lex_throw(ls, "unexpected symbol");
    return;
  }
}

/// \code
/// primaryExpr
///     : prefixExpr ('.' NAME | indexing | ':' NAME arguments | arguments)*
///     ;
/// \endcode
static void primaryExpr(LexState *ls, ExprInfo *v) {
  FuncState *fs = ls->fs;
  prefixExpr(ls, v);
  while (true) {
    switch (ls->t.token) {
    case '.':
      field(ls, v);
      break;
    case '[': {
      Codegen_exprToAnyReg(fs, v);
      ExprInfo key;
      indexing(ls, &key);
      Codegen_indexed(fs, v, &key);
      break;
    }
    case ':': {
      Lexer_next(ls);
      ExprInfo key;
      checkname(ls, &key);
      Codegen_self(fs, v, &key);
      arguments(ls, v);
      break;
    }
    case '(':
    case TK_STRING:
    case '{':
      Codegen_exprToNextReg(fs, v);
      arguments(ls, v);
      break;
    default:
      return;
    }
  }
}

/// \code
/// simpleExpr
///     : NUMBER
///     | STRING
///     | NIL
///     | true
///     | false
///     | ... # vararg
///     | constructor
///     | FUNCTION body
///     | primaryExpr
///     ;
/// \endcode
static void simpleExpr(LexState *ls, ExprInfo *v) {
  switch (ls->t.token) {
  case TK_NUMBER:
    numberLiteral(v, ls->t.literal.num);
    break;
  case TK_STRING:
    stringLiteral(ls, v, ls->t.literal.str);
    break;
  case TK_NIL:
    exprSetKind(v, EXPR_NIL);
    break;
  case TK_TRUE:
    exprSetKind(v, EXPR_TRUE);
    break;
  case TK_FALSE:
    exprSetKind(v, EXPR_FALSE);
    break;
  case TK_DOTS:
    // Vararg.
    FuncState *fs = ls->fs;
    if (!fs->f->varargMode) {
      Lex_throw(ls, "cannot use '...' outside a vararg function");
    }
    fs->f->varargMode &= ~VARARG_NEEDS_ARG; /* don't need 'arg' */
    exprSetKind(v, EXPR_VARARG_CALL);
    v->u.varargCallPC = Codegen_emitABC(fs, OP_VARARG, 0, 1, 0);
    break;
  case '{':
    // Constructor.
    constructor(ls, v);
    return;
  case TK_FUNCTION:
    Lexer_next(ls);
    body(ls, v, false, ls->linenumber);
    return;
  default:
    primaryExpr(ls, v);
    return;
  }
  Lexer_next(ls);
}

static OpKind unaryOp(int token) {
  switch (token) {
  case TK_NOT:
    return OPR_NOT;
  case '-':
    return OPR_MINUS;
  case '#':
    return OPR_LEN;
  default:
    return OPR_NONE;
  }
}

static OpKind binaryOp(int token) {
  switch (token) {
  case '+':
    return OPR_ADD;
  case '-':
    return OPR_SUB;
  case '*':
    return OPR_MUL;
  case '/':
    return OPR_DIV;
  case '%':
    return OPR_MOD;
  case '^':
    return OPR_POW;
  case TK_CONCAT:
    return OPR_CONCAT;
  case TK_NE:
    return OPR_NE;
  case TK_EQ:
    return OPR_EQ;
  case '<':
    return OPR_LT;
  case TK_LE:
    return OPR_LE;
  case '>':
    return OPR_GT;
  case TK_GE:
    return OPR_GE;
  case TK_AND:
    return OPR_AND;
  case TK_OR:
    return OPR_OR;
  default:
    return OPR_NONE;
  }
}

static const struct {
  uint8_t left;  /* left priority for each binary operator */
  uint8_t right; /* right priority */
} priority[] = {
    // Arithmetic.
    [OPR_ADD] = {6, 6},
    [OPR_SUB] = {6, 6},
    [OPR_MUL] = {7, 7},
    [OPR_DIV] = {7, 7},
    [OPR_MOD] = {7, 7},

    // Power and concat.
    [OPR_POW] = {10, 9},
    [OPR_CONCAT] = {5, 4},

    // Equality and inequality.
    [OPR_NE] = {3, 3},
    [OPR_EQ] = {3, 3},

    // Ordering.
    [OPR_LT] = {3, 3},
    [OPR_LE] = {3, 3},
    [OPR_GT] = {3, 3},
    [OPR_GE] = {3, 3},

    // Logical and/or.
    [OPR_AND] = {2, 2},
    [OPR_OR] = {1, 1},
};

#define UNARY_PRIORITY 8 /* priority for unary operators */

/// \code
/// subExpr
///     : (unaryOp subExpr | simpleExpr) (binaryOp subExpr)*
///     ;
/// \endcode
///
/// Accept binary operators with their priority greater than `minPriority`.
static OpKind subExpr(LexState *ls, ExprInfo *v, uint32_t minPriority) {
  enterLevel(ls);

  OpKind op = unaryOp(ls->t.token);
  if (op != OPR_NONE) {
    Lexer_next(ls);
    subExpr(ls, v, UNARY_PRIORITY);
    Codegen_prefix(ls->fs, op, v);
  } else {
    simpleExpr(ls, v);
  }

  /* expand while operators have priorities higher than `minPriority' */
  op = binaryOp(ls->t.token);
  while (op != OPR_NONE && priority[op].left > minPriority) {
    ExprInfo v2;
    OpKind nextop;
    Lexer_next(ls);
    Codegen_infix(ls->fs, op, v);
    /* read sub-expression with higher priority */
    nextop = subExpr(ls, &v2, priority[op].right);
    Codegen_suffix(ls->fs, op, v, &v2);
    op = nextop;
  }

  leaveLevel(ls);
  return op; /* return first untreated operator */
}

static void expr(LexState *ls, ExprInfo *v) { subExpr(ls, v, 0); }

static bool isBlockEnded(int token) {
  switch (token) {
  case TK_ELSE:
  case TK_ELSEIF:
  case TK_END:
  case TK_UNTIL:
  case TK_EOS:
    return true;
  default:
    return false;
  }
}

/// \code
/// block
///     : chunk
///     ;
/// \endcode
static void block(LexState *ls) {
  FuncState *fs = ls->fs;
  Block bl;
  enterBlock(fs, &bl, false);
  chunk(ls);
  assert(bl.breaklist == NO_JUMP);
  leaveBlock(fs);
}

/// Expressions that appear in the left-hand side of an assignment.
typedef struct LExpr {
  struct LExpr *prev;
  // Global, local, upvalue, or indexed variable.
  ExprInfo v;
} LExpr;

/*
** check whether, in an assignment to a local variable, the local variable
** is needed in a previous assignment (to a table). If so, save the original
** local value in a safe place and use this safe copy in the previous
** assignment.
*/
static void checkConflict(LexState *ls, LExpr *lhs, ExprInfo *v) {
  FuncState *fs = ls->fs;
  int extra = fs->freereg; /* eventual position to save local variable */
  bool conflict = false;
  assert(v->k == EXPR_LOCAL);
  for (; lhs; lhs = lhs->prev) {
    if (lhs->v.k == EXPR_INDEXED) {
      if (lhs->v.u.indexer.tableReg == v->u.localReg) { /* conflict? */
        conflict = true;
        /* previous assignment will use safe copy */
        lhs->v.u.indexer.tableReg = extra;
      }
      if (lhs->v.u.indexer.idxReg == v->u.localReg) { /* conflict? */
        conflict = true;
        /* previous assignment will use safe copy */
        lhs->v.u.indexer.idxReg = extra;
      }
    }
  }
  if (conflict) {
    // Make a copy.
    Codegen_emitABC(fs, OP_MOVE, fs->freereg, v->u.localReg, 0);
    luaK_reserveRegs(fs, 1);
  }
}

static void assignment(LexState *ls, LExpr *lh, int nvars) {
  ExprInfo e;
  if (!(EXPR_LOCAL <= lh->v.k && lh->v.k <= EXPR_INDEXED)) {
    Lex_throw(ls, "syntax error");
  }
  if (testNext(ls, ',')) { /* assignment -> `,' primaryExpr assignment */
    LExpr nv;
    nv.prev = lh;
    primaryExpr(ls, &nv.v);
    if (nv.v.k == EXPR_LOCAL) {
      checkConflict(ls, lh, &nv.v);
    }
    CHECK_LIMIT(ls->fs, nvars, LUAI_MAX_C_CALLS - ls->L->nestedCCallsNum,
                "variables in assignment");
    assignment(ls, &nv, nvars + 1);
  } else { /* assignment -> `=' exprList1 */
    int nexps;
    checkNext(ls, '=');
    nexps = exprList1(ls, &e);
    if (nexps != nvars) {
      adjust_assign(ls, nvars, nexps, &e);
      if (nexps > nvars) {
        ls->fs->freereg -= nexps - nvars; /* remove extra values */
      }
    } else {
      Codegen_setReturn(ls->fs, &e); /* close last expression */
      Codegen_storeVar(ls->fs, &lh->v, &e);
      return; /* avoid default */
    }
  }
  // Default assignment.
  exprSetKind(&e, EXPR_NON_RELOC);
  e.u.nonRelocReg = ls->fs->freereg - 1;
  Codegen_storeVar(ls->fs, &lh->v, &e);
}

static int cond(LexState *ls) {
  /* cond -> exp */
  ExprInfo v;
  expr(ls, &v); /* read condition */
  if (v.k == EXPR_NIL) {
    v.k = EXPR_FALSE; /* `falses' are all equal here */
  }
  Codegen_goIfTrue(ls->fs, &v);
  return v.f;
}

static void breakStmt(LexState *ls) {
  FuncState *fs = ls->fs;
  Block *bl = fs->bl;
  int upval = 0;
  while (bl && !bl->isBreakable) {
    upval |= bl->hasUpvalues;
    bl = bl->prev;
  }
  if (!bl) {
    Lex_throw(ls, "no loop to break");
  }
  if (upval) {
    Codegen_emitABC(fs, OP_CLOSE, bl->nactvar, 0, 0);
  }
  Codegen_concat(fs, &bl->breaklist, Codegen_jump(fs));
}

static void whileStmt(LexState *ls, int line) {
  /* whileStmt -> WHILE cond DO block END */
  FuncState *fs = ls->fs;
  int whileinit;
  int condexit;
  Block bl;
  Lexer_next(ls); /* skip WHILE */
  whileinit = Codegen_getLabel(fs);
  condexit = cond(ls);
  enterBlock(fs, &bl, true);
  checkNext(ls, TK_DO);
  block(ls);
  Codegen_patchList(fs, Codegen_jump(fs), whileinit);
  check_match(ls, TK_END, TK_WHILE, line);
  leaveBlock(fs);
  Codegen_patchTo(fs, condexit); /* false conditions finish the loop */
}

static void repeatStmt(LexState *ls, int line) {
  /* repeatStmt -> REPEAT block UNTIL cond */
  int condexit;
  FuncState *fs = ls->fs;
  int repeat_init = Codegen_getLabel(fs);
  Block bl1, bl2;
  enterBlock(fs, &bl1, true);  /* loop block */
  enterBlock(fs, &bl2, false); /* scope block */
  Lexer_next(ls);              /* skip REPEAT */
  chunk(ls);
  check_match(ls, TK_UNTIL, TK_REPEAT, line);
  condexit = cond(ls);    /* read condition (inside scope block) */
  if (!bl2.hasUpvalues) { /* no upvalues? */
    leaveBlock(fs);       /* finish scope */
    Codegen_patchList(ls->fs, condexit, repeat_init); /* close the loop */
  } else {         /* complete semantics when there are upvalues */
    breakStmt(ls); /* if condition then break */
    Codegen_patchTo(ls->fs, condexit); /* else... */
    leaveBlock(fs);                    /* finish scope... */
    Codegen_patchList(ls->fs, Codegen_jump(fs), repeat_init); /* and repeat */
  }
  leaveBlock(fs); /* finish loop */
}

static int exp1(LexState *ls) {
  ExprInfo e;
  int k;
  expr(ls, &e);
  k = e.k;
  Codegen_exprToNextReg(ls->fs, &e);
  return k;
}

static void forbody(LexState *ls, int base, int line, int nvars, int isnum) {
  /* forbody -> DO block */
  Block bl;
  FuncState *fs = ls->fs;
  int prep, endfor;
  adjustlocalvars(ls, 3); /* control variables */
  checkNext(ls, TK_DO);
  prep =
      isnum ? luaK_codeAsBx(fs, OP_FORPREP, base, NO_JUMP) : Codegen_jump(fs);
  enterBlock(fs, &bl, false); /* scope for declared variables */
  adjustlocalvars(ls, nvars);
  luaK_reserveRegs(fs, nvars);
  block(ls);
  leaveBlock(fs); /* end of scope for declared variables */
  Codegen_patchTo(fs, prep);
  endfor = (isnum) ? luaK_codeAsBx(fs, OP_FORLOOP, base, NO_JUMP)
                   : Codegen_emitABC(fs, OP_TFORLOOP, base, 0, nvars);
  Codegen_fixLine(fs, line); /* pretend that `OP_FOR' starts the loop */
  Codegen_patchList(fs, (isnum ? endfor : Codegen_jump(fs)), prep + 1);
}

static void fornum(LexState *ls, String *varname, int line) {
  /* fornum -> NAME = exp1,exp1[,exp1] forbody */
  FuncState *fs = ls->fs;
  int base = fs->freereg;
  new_localvarliteral(ls, "(for index)", 0);
  new_localvarliteral(ls, "(for limit)", 1);
  new_localvarliteral(ls, "(for step)", 2);
  new_localvar(ls, varname, 3);
  checkNext(ls, '=');
  exp1(ls); /* initial value */
  checkNext(ls, ',');
  exp1(ls); /* limit */
  if (testNext(ls, ',')) {
    exp1(ls); /* optional step */
  } else {    /* default step = 1 */
    Codegen_emitABx(fs, OP_LOADK, fs->freereg, Codegen_addNumber(fs, 1));
    luaK_reserveRegs(fs, 1);
  }
  forbody(ls, base, line, 1, 1);
}

static void forlist(LexState *ls, String *indexname) {
  /* forlist -> NAME {,NAME} IN exprList1 forbody */
  FuncState *fs = ls->fs;
  ExprInfo e;
  int nvars = 0;
  int line;
  int base = fs->freereg;
  /* create control variables */
  new_localvarliteral(ls, "(for generator)", nvars++);
  new_localvarliteral(ls, "(for state)", nvars++);
  new_localvarliteral(ls, "(for control)", nvars++);
  /* create declared variables */
  new_localvar(ls, indexname, nvars++);
  while (testNext(ls, ',')) {
    new_localvar(ls, checkName(ls), nvars++);
  }
  checkNext(ls, TK_IN);
  line = ls->linenumber;
  adjust_assign(ls, 3, exprList1(ls, &e), &e);
  Codegen_reserveStack(fs, 3); /* extra space to call generator */
  forbody(ls, base, line, nvars - 3, 0);
}

static void forStmt(LexState *ls, int line) {
  /* forStmt -> FOR (fornum | forlist) END */
  FuncState *fs = ls->fs;
  String *varname;
  Block bl;
  enterBlock(fs, &bl, true); /* scope for loop and control variables */
  Lexer_next(ls);            /* skip `for' */
  varname = checkName(ls);   /* first variable name */
  switch (ls->t.token) {
  case '=':
    fornum(ls, varname, line);
    break;
  case ',':
  case TK_IN:
    forlist(ls, varname);
    break;
  default:
    Lex_throw(ls, "'=' or 'in' expected");
  }
  check_match(ls, TK_END, TK_FOR, line);
  leaveBlock(fs); /* loop scope (`break' jumps to this point) */
}

static int test_then_block(LexState *ls) {
  /* test_then_block -> [IF | ELSEIF] cond THEN block */
  int condexit;
  Lexer_next(ls); /* skip IF or ELSEIF */
  condexit = cond(ls);
  checkNext(ls, TK_THEN);
  block(ls); /* `then' part */
  return condexit;
}

static void ifStmt(LexState *ls, int line) {
  /* ifStmt -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END
   */
  FuncState *fs = ls->fs;
  int flist;
  int escapelist = NO_JUMP;
  flist = test_then_block(ls); /* IF cond THEN block */
  while (ls->t.token == TK_ELSEIF) {
    Codegen_concat(fs, &escapelist, Codegen_jump(fs));
    Codegen_patchTo(fs, flist);
    flist = test_then_block(ls); /* ELSEIF cond THEN block */
  }
  if (ls->t.token == TK_ELSE) {
    Codegen_concat(fs, &escapelist, Codegen_jump(fs));
    Codegen_patchTo(fs, flist);
    Lexer_next(ls); /* skip ELSE (after patch, for correct line info) */
    block(ls);      /* `else' part */
  } else {
    Codegen_concat(fs, &escapelist, flist);
  }
  Codegen_patchTo(fs, escapelist);
  check_match(ls, TK_END, TK_IF, line);
}

static void localFuncStmt(LexState *ls) {
  ExprInfo v, b;
  FuncState *fs = ls->fs;
  new_localvar(ls, checkName(ls), 0);
  exprSetKind(&v, EXPR_LOCAL);
  v.u.localReg = fs->freereg;
  luaK_reserveRegs(fs, 1);
  adjustlocalvars(ls, 1);
  body(ls, &b, false, ls->linenumber);
  Codegen_storeVar(fs, &v, &b);
  /* debug information will only see the variable after this point! */
  GET_LOCAL_VAR(fs, fs->nactvar - 1).startPC = fs->pc;
}

static void localStmt(LexState *ls) {
  /* stat -> LOCAL NAME {`,' NAME} [`=' exprList1] */
  int nvars = 0;
  int nexps;
  ExprInfo e;
  do {
    new_localvar(ls, checkName(ls), nvars++);
  } while (testNext(ls, ','));
  if (testNext(ls, '=')) {
    nexps = exprList1(ls, &e);
  } else {
    e.k = EXPR_VOID;
    nexps = 0;
  }
  adjust_assign(ls, nvars, nexps, &e);
  adjustlocalvars(ls, nvars);
}

static bool funcname(LexState *ls, ExprInfo *v) {
  /* funcname -> NAME {field} [`:' NAME] */
  bool hasSelf = false;
  singleVar(ls, v);
  while (ls->t.token == '.') {
    field(ls, v);
  }
  if (ls->t.token == ':') {
    hasSelf = true;
    field(ls, v);
  }
  return hasSelf;
}

static void funcStmt(LexState *ls, int line) {
  /* funcStmt -> FUNCTION funcname body */
  Lexer_next(ls); /* skip FUNCTION */
  ExprInfo v;
  bool hasSelf = funcname(ls, &v);
  ExprInfo b;
  body(ls, &b, hasSelf, line);
  Codegen_storeVar(ls->fs, &v, &b);
  Codegen_fixLine(ls->fs, line); /* definition `happens' in the first line */
}

/// \code
/// exprStmt
///     : primaryExpr assignment?
///     ;
/// \endcode
///
/// Note that assignment is only valid when the former is not a function call.
static void exprStmt(LexState *ls) {
  LExpr v;
  primaryExpr(ls, &v.v);
  if (v.v.k == EXPR_CALL) {
    // Call stmt uses no results.
    SETARG_C(ls->fs->f->code[v.v.u.callPC], 1);
  } else {
    v.prev = nullptr;
    assignment(ls, &v, 1);
  }
}

/// \code
/// returnStmt
///     : RETURN exprList1
///     ;
/// \endcode
static void returnStmt(LexState *ls) {
  Lexer_next(ls); // skip RETURN

  if (isBlockEnded(ls->t.token) || ls->t.token == ';') {
    // Returns no values.
    Codegen_return(ls->fs, 0, 0);
    return;
  }

  FuncState *fs = ls->fs;
  ExprInfo e;
  int nret = exprList1(ls, &e);

  if (HAS_MULTI_RETURN(e.k)) {
    Codegen_setReturnMulti(fs, &e, LUA_MULTRET);
    if (e.k == EXPR_CALL && nret == 1) {
      SET_OPCODE(fs->f->code[e.u.callPC], OP_TAILCALL);
      assert(GETARG_A(fs->f->code[e.u.callPC]) == fs->nactvar);
    }
    Codegen_return(fs, fs->nactvar, LUA_MULTRET);
    return;
  }

  int first; // registers with returned values
  if (nret == 1) {
    first = Codegen_exprToAnyReg(fs, &e);
  } else {
    // Values must go to the stack.
    Codegen_exprToNextReg(fs, &e);
    // Returns all active values.
    first = fs->nactvar;
    assert(nret == fs->freereg - first);
  }
  Codegen_return(fs, first, nret);
}

/// \code
/// stmt
///     : ifStmt
///     | whileStmt
///     | DO block END
///     | forStmt
///     | repeatStmt
///     | funcStmt
///     | localFuncStmt
///     | localStmt
///     | returnStmt
///     | breakStmt
///     | exprStmt
///     ;
/// \endcode
static bool stmt(LexState *ls) {
  int line = ls->linenumber; // may be needed for error messages
  switch (ls->t.token) {
  case TK_IF:
    ifStmt(ls, line); // IF skipped inside, not here
    return false;
  case TK_WHILE:
    whileStmt(ls, line); // WHILE skipped inside, not here
    return false;
  case TK_DO:
    Lexer_next(ls); // skip DO
    block(ls);
    check_match(ls, TK_END, TK_DO, line);
    return false;
  case TK_FOR:
    forStmt(ls, line); // FOR skipped inside, not here
    return false;
  case TK_REPEAT:
    repeatStmt(ls, line); // REPEAT skipped inside, not here
    return false;
  case TK_FUNCTION:
    funcStmt(ls, line);
    return false;
  case TK_LOCAL:
    Lexer_next(ls); // skip LOCAL
    if (testNext(ls, TK_FUNCTION)) {
      localFuncStmt(ls);
    } else {
      localStmt(ls);
    }
    return false;
  case TK_RETURN:
    returnStmt(ls); // RETURN skipped inside, not here
    return true;    // must be last stmt
  case TK_BREAK:
    Lexer_next(ls); // skip BREAK
    breakStmt(ls);
    return true; // must be last stmt
  default:
    exprStmt(ls);
    return false;
  }
}

/// \code
/// chunk
///     : stmt ';'?
///     ;
/// \endcode
static void chunk(LexState *ls) {
  bool isLast = false;
  enterLevel(ls);
  while (!isLast && !isBlockEnded(ls->t.token)) {
    isLast = stmt(ls);
    testNext(ls, ';');
    assert(ls->fs->f->maxStackSize >= ls->fs->freereg &&
           ls->fs->freereg >= ls->fs->nactvar);
    ls->fs->freereg = ls->fs->nactvar; /* free registers */
  }
  leaveLevel(ls);
}
