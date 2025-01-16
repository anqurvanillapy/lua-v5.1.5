/* Lua Parser. */

#include <string.h>

#include "lua.h"

#include "intern.h"
#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "llex.h"
#include "lmem.h"
#include "lopcodes.h"
#include "lstate.h"
#include "ltable.h"
#include "object.h"
#include "parser.h"

#define HAS_MULTI_RETURN(k) ((k) == VCALL || (k) == VVARARG)

#define getlocvar(fs, i) ((fs)->f->locVars[(fs)->actvar[i]])

#define luaY_checklimit(fs, v, l, m)                                           \
  if ((v) > (l))                                                               \
  errorlimit(fs, l, m)

/*
** nodes for block list (list of active blocks)
*/
typedef struct BlockCnt {
  struct BlockCnt *previous; /* chain */
  int breaklist;             /* list of jumps out of this loop */
  uint8_t nactvar;     /* # active locals outside the breakable structure */
  uint8_t upval;       /* true if some variable in the block is an upvalue */
  uint8_t isbreakable; /* true if `block' is a loop */
} BlockCnt;

/*
** prototypes for recursive non-terminal functions
*/
static void chunk(LexState *ls);
static void expr(LexState *ls, ExprInfo *v);

static void anchor_token(LexState *ls) {
  if (ls->t.token == TK_NAME || ls->t.token == TK_STRING) {
    StringHeader *ts = ls->t.literal.str;
    luaX_newstring(ls, STRING_CONTENT(ts), ts->len);
  }
}

static void error_expected(LexState *ls, int token) {
  luaX_syntaxerror(ls, luaO_pushfstring(ls->L, LUA_QS " expected",
                                        luaX_token2str(ls, token)));
}

static void errorlimit(FuncState *fs, int limit, const char *what) {
  const char *msg =
      (fs->f->lineDefined == 0)
          ? luaO_pushfstring(fs->L, "main function has more than %d %s", limit,
                             what)
          : luaO_pushfstring(fs->L, "function at line %d has more than %d %s",
                             fs->f->lineDefined, limit, what);
  luaX_lexerror(fs->ls, msg, 0);
}

static bool testNext(LexState *ls, int c) {
  if (ls->t.token == c) {
    luaX_next(ls);
    return true;
  }
  return false;
}

static void check(LexState *ls, int c) {
  if (ls->t.token != c) {
    error_expected(ls, c);
  }
}

static void checknext(LexState *ls, int c) {
  check(ls, c);
  luaX_next(ls);
}

#define check_condition(ls, c, msg)                                            \
  do {                                                                         \
    if (!(c)) {                                                                \
      luaX_syntaxerror(ls, msg);                                               \
    }                                                                          \
  } while (false)

static void check_match(LexState *ls, int what, int who, int where) {
  if (!testNext(ls, what)) {
    if (where == ls->linenumber) {
      error_expected(ls, what);
    } else {
      luaX_syntaxerror(
          ls, luaO_pushfstring(
                  ls->L, LUA_QS " expected (to close " LUA_QS " at line %d)",
                  luaX_token2str(ls, what), luaX_token2str(ls, who), where));
    }
  }
}

static StringHeader *str_checkname(LexState *ls) {
  check(ls, TK_NAME);
  StringHeader *ts = ls->t.literal.str;
  luaX_next(ls);
  return ts;
}

static void exprSetInfo(ExprInfo *e, ExprKind k, int info) {
  e->f = NO_JUMP;
  e->t = NO_JUMP;
  e->k = k;
  e->u.s.info = info;
}

static void exprSetKind(ExprInfo *e, ExprKind k) {
  e->f = NO_JUMP;
  e->t = NO_JUMP;
  e->k = k;
}

static void numberLiteral(ExprInfo *e, lua_Number value) {
  exprSetKind(e, VKNUM);
  e->u.value = value;
}

static void stringLiteral(LexState *ls, ExprInfo *e, StringHeader *s) {
  exprSetInfo(e, VK, Codegen_addString(ls->fs, s));
}

static void checkname(LexState *ls, ExprInfo *e) {
  stringLiteral(ls, e, str_checkname(ls));
}

static int registerlocalvar(LexState *ls, StringHeader *varname) {
  FuncState *fs = ls->fs;
  Prototype *f = fs->f;
  int oldsize = f->locVarsSize;
  luaM_growvector(ls->L, f->locVars, fs->nlocvars, f->locVarsSize, LocVar,
                  SHRT_MAX, "too many local variables");
  while (oldsize < f->locVarsSize) {
    f->locVars[oldsize++].varname = nullptr;
  }
  f->locVars[fs->nlocvars].varname = varname;
  luaC_objbarrier(ls->L, f, varname);
  return fs->nlocvars++;
}

#define new_localvarliteral(ls, v, n)                                          \
  new_localvar(ls, luaX_newstring(ls, "" v, (sizeof(v) / sizeof(char)) - 1), n)

static void new_localvar(LexState *ls, StringHeader *name, int n) {
  FuncState *fs = ls->fs;
  luaY_checklimit(fs, fs->nactvar + n + 1, LUAI_MAX_VARS, "local variables");
  fs->actvar[fs->nactvar + n] =
      cast(unsigned short, registerlocalvar(ls, name));
}

static void adjustlocalvars(LexState *ls, int nvars) {
  FuncState *fs = ls->fs;
  fs->nactvar = cast_byte(fs->nactvar + nvars);
  for (; nvars; nvars--) {
    getlocvar(fs, fs->nactvar - nvars).startPC = fs->pc;
  }
}

static void removevars(LexState *ls, int tolevel) {
  FuncState *fs = ls->fs;
  while (fs->nactvar > tolevel) {
    getlocvar(fs, --fs->nactvar).endPC = fs->pc;
  }
}

static int indexupvalue(FuncState *fs, StringHeader *name, ExprInfo *v) {
  int i;
  Prototype *f = fs->f;
  int oldsize = f->upvaluesSize;
  for (i = 0; i < f->upvaluesNum; i++) {
    if (fs->upvalues[i].k == v->k && fs->upvalues[i].info == v->u.s.info) {
      DEBUG_ASSERT(f->upvalues[i] == name);
      return i;
    }
  }
  /* new one */
  luaY_checklimit(fs, f->upvaluesNum + 1, LUAI_MAX_UPVALUES, "upvalues");
  luaM_growvector(fs->L, f->upvalues, f->upvaluesNum, f->upvaluesSize,
                  StringHeader *, MAX_INT, "");
  while (oldsize < f->upvaluesSize) {
    f->upvalues[oldsize++] = nullptr;
  }
  f->upvalues[f->upvaluesNum] = name;
  luaC_objbarrier(fs->L, f, name);
  DEBUG_ASSERT(v->k == VLOCAL || v->k == VUPVAL);
  fs->upvalues[f->upvaluesNum].k = v->k;
  fs->upvalues[f->upvaluesNum].info = v->u.s.info;
  return f->upvaluesNum++;
}

static int searchvar(FuncState *fs, StringHeader *n) {
  int i;
  for (i = fs->nactvar - 1; i >= 0; i--) {
    if (n == getlocvar(fs, i).varname) {
      return i;
    }
  }
  return -1; /* not found */
}

static void markupval(FuncState *fs, int level) {
  BlockCnt *bl = fs->bl;
  while (bl && bl->nactvar > level) {
    bl = bl->previous;
  }
  if (bl) {
    bl->upval = 1;
  }
}

static int singlevaraux(FuncState *fs, StringHeader *n, ExprInfo *var,
                        int base) {
  if (fs == nullptr) {                 /* no more levels? */
    exprSetInfo(var, VGLOBAL, NO_REG); /* default is global variable */
    return VGLOBAL;
  } else {
    int v = searchvar(fs, n); /* look up at current level */
    if (v >= 0) {
      exprSetInfo(var, VLOCAL, v);
      if (!base) {
        markupval(fs, v); /* local will be used as an upval */
      }
      return VLOCAL;
    } else { /* not found at current level; try upper one */
      if (singlevaraux(fs->prev, n, var, 0) == VGLOBAL) {
        return VGLOBAL;
      }
      var->u.s.info = indexupvalue(fs, n, var); /* else was LOCAL or UPVAL */
      var->k = VUPVAL;                          /* upvalue in this level */
      return VUPVAL;
    }
  }
}

static void singlevar(LexState *ls, ExprInfo *var) {
  StringHeader *varname = str_checkname(ls);
  FuncState *fs = ls->fs;
  if (singlevaraux(fs, varname, var, 1) == VGLOBAL) {
    // Info points to global name.
    var->u.s.info = Codegen_addString(fs, varname);
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
    Codegen_setReturnMulti(fs, e,
                           extra); /* last exp. provides the difference */
    if (extra > 1) {
      luaK_reserveregs(fs, extra - 1);
    }
  } else {
    if (e->k != VVOID) {
      luaK_exp2nextreg(fs, e); /* close last expression */
    }
    if (extra > 0) {
      int reg = fs->freereg;
      luaK_reserveregs(fs, extra);
      luaK_nil(fs, reg, extra);
    }
  }
}

static void enterLevel(LexState *ls) {
  ls->L->nestedCCallsNum++;
  if (ls->L->nestedCCallsNum > LUAI_MAX_C_CALLS) {
    luaX_lexerror(ls, "chunk has too many syntax levels", 0);
  }
}

#define leaveLevel(ls) ((ls)->L->nestedCCallsNum--)

static void enterBlock(FuncState *fs, BlockCnt *bl, uint8_t isbreakable) {
  bl->breaklist = NO_JUMP;
  bl->isbreakable = isbreakable;
  bl->nactvar = fs->nactvar;
  bl->upval = 0;
  bl->previous = fs->bl;
  fs->bl = bl;
  DEBUG_ASSERT(fs->freereg == fs->nactvar);
}

static void leaveBlock(FuncState *fs) {
  BlockCnt *bl = fs->bl;
  fs->bl = bl->previous;
  removevars(fs->ls, bl->nactvar);
  if (bl->upval) {
    luaK_codeABC(fs, OP_CLOSE, bl->nactvar, 0, 0);
  }
  /* a block either controls scope or breaks (never both) */
  DEBUG_ASSERT(!bl->isbreakable || !bl->upval);
  DEBUG_ASSERT(bl->nactvar == fs->nactvar);
  fs->freereg = fs->nactvar; /* free registers */
  luaK_patchtohere(fs, bl->breaklist);
}

static void pushclosure(LexState *ls, FuncState *func, ExprInfo *v) {
  FuncState *fs = ls->fs;
  Prototype *f = fs->f;
  int oldsize = f->pSize;
  int i;
  luaM_growvector(ls->L, f->inners, fs->np, f->pSize, Prototype *, MAXARG_Bx,
                  "constant table overflow");
  while (oldsize < f->pSize) {
    f->inners[oldsize++] = nullptr;
  }
  f->inners[fs->np++] = func->f;
  luaC_objbarrier(ls->L, f, func->f);
  exprSetInfo(v, VRELOCABLE, luaK_codeABx(fs, OP_CLOSURE, 0, fs->np - 1));
  for (i = 0; i < func->f->upvaluesNum; i++) {
    OpCode o = (func->upvalues[i].k == VLOCAL) ? OP_MOVE : OP_GETUPVAL;
    luaK_codeABC(fs, o, 0, func->upvalues[i].info, 0);
  }
}

static void openFunc(LexState *ls, FuncState *fs) {
  lua_State *L = ls->L;
  Prototype *f = luaF_newproto(L);
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
  fs->h = luaH_new(L, 0, 0);
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
  luaK_ret(fs, 0, 0); /* final return */
  luaM_reallocvector(L, f->code, f->codeSize, fs->pc, Instruction);
  f->codeSize = fs->pc;
  luaM_reallocvector(L, f->lineInfo, f->lineInfoSize, fs->pc, int);
  f->lineInfoSize = fs->pc;
  luaM_reallocvector(L, f->k, f->kSize, fs->nk, Value);
  f->kSize = fs->nk;
  luaM_reallocvector(L, f->inners, f->pSize, fs->np, Prototype *);
  f->pSize = fs->np;
  luaM_reallocvector(L, f->locVars, f->locVarsSize, fs->nlocvars, LocVar);
  f->locVarsSize = fs->nlocvars;
  luaM_reallocvector(L, f->upvalues, f->upvaluesSize, f->upvaluesNum,
                     StringHeader *);
  f->upvaluesSize = f->upvaluesNum;
  DEBUG_ASSERT(luaG_checkcode(f));
  DEBUG_ASSERT(fs->bl == nullptr);
  ls->fs = fs->prev;
  /* last token read was anchored in defunct function; must re-anchor it */
  anchor_token(ls);
  L->top -= 2; /* remove table and prototype from the stack */
}

Prototype *luaY_parser(lua_State *L, ZIO *z, Mbuffer *buff, const char *name) {
  struct LexState lexstate;
  struct FuncState funcstate;
  lexstate.buff = buff;
  luaX_setinput(L, &lexstate, z, String_internCStr(L, name));
  openFunc(&lexstate, &funcstate);
  funcstate.f->varargMode = VARARG_IS_VARARG; /* main func. is always vararg */
  luaX_next(&lexstate);                       /* read first token */
  chunk(&lexstate);
  check(&lexstate, TK_EOS);
  closeFunc(&lexstate);
  DEBUG_ASSERT(funcstate.prev == nullptr);
  DEBUG_ASSERT(funcstate.f->upvaluesNum == 0);
  DEBUG_ASSERT(lexstate.fs == nullptr);
  return funcstate.f;
}

static void field(LexState *ls, ExprInfo *v) {
  /* field -> ['.' | ':'] NAME */
  FuncState *fs = ls->fs;
  ExprInfo key;
  luaK_exp2anyreg(fs, v);
  luaX_next(ls); /* skip the dot or colon */
  checkname(ls, &key);
  luaK_indexed(fs, v, &key);
}

static void yindex(LexState *ls, ExprInfo *v) {
  /* index -> '[' expr ']' */
  luaX_next(ls); /* skip the '[' */
  expr(ls, v);
  luaK_exp2val(ls->fs, v);
  checknext(ls, ']');
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
    luaY_checklimit(fs, cc->nh, MAX_INT, "items in a constructor");
    checkname(ls, &key);
  } else { /* ls->t.token == '[' */
    yindex(ls, &key);
  }
  cc->nh++;
  checknext(ls, '=');
  rkkey = luaK_exp2RK(fs, &key);
  expr(ls, &val);
  luaK_codeABC(fs, OP_SETTABLE, cc->t->u.s.info, rkkey, luaK_exp2RK(fs, &val));
  fs->freereg = reg; /* free registers */
}

static void closelistfield(FuncState *fs, struct ConsControl *cc) {
  if (cc->v.k == VVOID) {
    return; /* there is no list item */
  }
  luaK_exp2nextreg(fs, &cc->v);
  cc->v.k = VVOID;
  if (cc->tostore == LFIELDS_PER_FLUSH) {
    luaK_setlist(fs, cc->t->u.s.info, cc->na, cc->tostore); /* flush */
    cc->tostore = 0; /* no more items pending */
  }
}

static void lastlistfield(FuncState *fs, struct ConsControl *cc) {
  if (cc->tostore == 0) {
    return;
  }
  if (HAS_MULTI_RETURN(cc->v.k)) {
    Codegen_setReturnMulti(fs, &cc->v, LUA_MULTRET);
    luaK_setlist(fs, cc->t->u.s.info, cc->na, LUA_MULTRET);
    cc->na--; /* do not count last expression (unknown number of elements) */
  } else {
    if (cc->v.k != VVOID) {
      luaK_exp2nextreg(fs, &cc->v);
    }
    luaK_setlist(fs, cc->t->u.s.info, cc->na, cc->tostore);
  }
}

static void listfield(LexState *ls, struct ConsControl *cc) {
  expr(ls, &cc->v);
  luaY_checklimit(ls->fs, cc->na, MAX_INT, "items in a constructor");
  cc->na++;
  cc->tostore++;
}

static void constructor(LexState *ls, ExprInfo *t) {
  /* constructor -> ?? */
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  int pc = luaK_codeABC(fs, OP_NEWTABLE, 0, 0, 0);
  struct ConsControl cc;
  cc.na = cc.nh = cc.tostore = 0;
  cc.t = t;
  exprSetInfo(t, VRELOCABLE, pc);
  exprSetInfo(&cc.v, VVOID, 0); /* no value (yet) */
  luaK_exp2nextreg(ls->fs, t);  /* fix it at stack top (for gc) */
  checknext(ls, '{');
  do {
    DEBUG_ASSERT(cc.v.k == VVOID || cc.tostore > 0);
    if (ls->t.token == '}') {
      break;
    }
    closelistfield(fs, &cc);
    switch (ls->t.token) {
    case TK_NAME: { /* may be listfields or recfields */
      luaX_lookahead(ls);
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
        new_localvar(ls, str_checkname(ls), nparams++);
        break;
      }
      case TK_DOTS: { /* param -> `...' */
        luaX_next(ls);

        // Compatible with the old-style variadic arguments: Use `arg` as the
        // default name.
        new_localvarliteral(ls, "arg", nparams++);
        f->varargMode = VARARG_HAS_ARG | VARARG_NEEDS_ARG;

        f->varargMode |= VARARG_IS_VARARG;
        break;
      }
      default:
        luaX_syntaxerror(ls, "<name> or " LUA_QL("...") " expected");
      }
    } while (!f->varargMode && testNext(ls, ','));
  }
  adjustlocalvars(ls, nparams);
  f->paramsNum = cast_byte(fs->nactvar - (f->varargMode & VARARG_HAS_ARG));
  luaK_reserveregs(fs, fs->nactvar); /* reserve register for parameters */
}

static void body(LexState *ls, ExprInfo *e, int needself, int line) {
  /* body ->  `(' parlist `)' chunk END */
  FuncState new_fs;
  openFunc(ls, &new_fs);
  new_fs.f->lineDefined = line;
  checknext(ls, '(');
  if (needself) {
    new_localvarliteral(ls, "self", 0);
    adjustlocalvars(ls, 1);
  }
  parlist(ls);
  checknext(ls, ')');
  chunk(ls);
  new_fs.f->lineDefinedLast = ls->linenumber;
  check_match(ls, TK_END, TK_FUNCTION, line);
  closeFunc(ls);
  pushclosure(ls, &new_fs, e);
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
    luaK_exp2nextreg(ls->fs, v);
    expr(ls, v);
    exprNum++;
  }
  return exprNum;
}

static void funcargs(LexState *ls, ExprInfo *f) {
  FuncState *fs = ls->fs;
  ExprInfo args;
  int base, nparams;
  int line = ls->linenumber;
  switch (ls->t.token) {
  case '(': { /* funcargs -> `(' [ exprList1 ] `)' */
    if (line != ls->lastline) {
      luaX_syntaxerror(ls, "ambiguous syntax (function call x new stmt)");
    }
    luaX_next(ls);
    if (ls->t.token == ')') { /* arg list is empty? */
      args.k = VVOID;
    } else {
      exprList1(ls, &args);
      Codegen_setReturnMulti(fs, &args, LUA_MULTRET);
    }
    check_match(ls, ')', '(', line);
    break;
  }
  case '{': { /* funcargs -> constructor */
    constructor(ls, &args);
    break;
  }
  case TK_STRING: { /* funcargs -> STRING */
    stringLiteral(ls, &args, ls->t.literal.str);
    luaX_next(ls); /* must use `literal' before `next' */
    break;
  }
  default: {
    luaX_syntaxerror(ls, "function arguments expected");
    return;
  }
  }
  DEBUG_ASSERT(f->k == VNONRELOC);
  base = f->u.s.info; /* base register for call */
  if (HAS_MULTI_RETURN(args.k)) {
    nparams = LUA_MULTRET; /* open call */
  } else {
    if (args.k != VVOID) {
      luaK_exp2nextreg(fs, &args); /* close last argument */
    }
    nparams = fs->freereg - (base + 1);
  }
  exprSetInfo(f, VCALL, luaK_codeABC(fs, OP_CALL, base, nparams + 1, 2));
  luaK_fixline(fs, line);
  fs->freereg = base + 1; /* call remove function and arguments and leaves
                             (unless changed) one result */
}

static void prefixexp(LexState *ls, ExprInfo *v) {
  /* prefixexp -> NAME | '(' expr ')' */
  switch (ls->t.token) {
  case '(': {
    int line = ls->linenumber;
    luaX_next(ls);
    expr(ls, v);
    check_match(ls, ')', '(', line);
    luaK_dischargevars(ls->fs, v);
    return;
  }
  case TK_NAME: {
    singlevar(ls, v);
    return;
  }
  default: {
    luaX_syntaxerror(ls, "unexpected symbol");
    return;
  }
  }
}

static void primaryexp(LexState *ls, ExprInfo *v) {
  /* primaryexp ->
        prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs } */
  FuncState *fs = ls->fs;
  prefixexp(ls, v);
  for (;;) {
    switch (ls->t.token) {
    case '.': { /* field */
      field(ls, v);
      break;
    }
    case '[': { /* `[' exp1 `]' */
      ExprInfo key;
      luaK_exp2anyreg(fs, v);
      yindex(ls, &key);
      luaK_indexed(fs, v, &key);
      break;
    }
    case ':': { /* `:' NAME funcargs */
      ExprInfo key;
      luaX_next(ls);
      checkname(ls, &key);
      luaK_self(fs, v, &key);
      funcargs(ls, v);
      break;
    }
    case '(':
    case TK_STRING:
    case '{': { /* funcargs */
      luaK_exp2nextreg(fs, v);
      funcargs(ls, v);
      break;
    }
    default:
      return;
    }
  }
}

/// \code
/// simpleexp
///     : NUMBER
///     | STRING
///     | NIL
///     | true
///     | false
///     | ... # vararg
///     | constructor
///     | FUNCTION body
///     | primaryexp
///     ;
/// \endcode
static void simpleexp(LexState *ls, ExprInfo *v) {
  switch (ls->t.token) {
  case TK_NUMBER:
    numberLiteral(v, ls->t.literal.num);
    break;
  case TK_STRING:
    stringLiteral(ls, v, ls->t.literal.str);
    break;
  case TK_NIL:
    exprSetKind(v, VNIL);
    break;
  case TK_TRUE:
    exprSetKind(v, VTRUE);
    break;
  case TK_FALSE:
    exprSetKind(v, VFALSE);
    break;
  case TK_DOTS: {
    // Vararg.
    FuncState *fs = ls->fs;
    if (!fs->f->varargMode) {
      luaX_syntaxerror(
          ls, "cannot use " LUA_QL("...") " outside a vararg function");
    }
    fs->f->varargMode &= ~VARARG_NEEDS_ARG; /* don't need 'arg' */
    exprSetInfo(v, VVARARG, luaK_codeABC(fs, OP_VARARG, 0, 1, 0));
    break;
  }
  case '{':
    // Constructor.
    constructor(ls, v);
    return;
  case TK_FUNCTION:
    luaX_next(ls);
    body(ls, v, 0, ls->linenumber);
    return;
  default:
    primaryexp(ls, v);
    return;
  }
  luaX_next(ls);
}

static UnOpr getunopr(int op) {
  switch (op) {
  case TK_NOT:
    return OPR_NOT;
  case '-':
    return OPR_MINUS;
  case '#':
    return OPR_LEN;
  default:
    return OPR_NOUNOPR;
  }
}

static BinOpr getbinopr(int op) {
  switch (op) {
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
    return OPR_NOBINOPR;
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
/// subexpr
///     : (simpleexp | unop subexpr) (binop subexpr)*
///     ;
/// \endcode
///
/// Where `binop` is any binary operator with a priority higher than
/// `minPriority`.
static BinOpr subexpr(LexState *ls, ExprInfo *v, unsigned int minPriority) {
  enterLevel(ls);

  UnOpr uop = getunopr(ls->t.token);
  if (uop != OPR_NOUNOPR) {
    luaX_next(ls);
    subexpr(ls, v, UNARY_PRIORITY);
    luaK_prefix(ls->fs, uop, v);
  } else {
    simpleexp(ls, v);
  }

  /* expand while operators have priorities higher than `minPriority' */
  BinOpr op = getbinopr(ls->t.token);
  while (op != OPR_NOBINOPR && priority[op].left > minPriority) {
    ExprInfo v2;
    BinOpr nextop;
    luaX_next(ls);
    luaK_infix(ls->fs, op, v);
    /* read sub-expression with higher priority */
    nextop = subexpr(ls, &v2, priority[op].right);
    luaK_posfix(ls->fs, op, v, &v2);
    op = nextop;
  }

  leaveLevel(ls);
  return op; /* return first untreated operator */
}

static void expr(LexState *ls, ExprInfo *v) { subexpr(ls, v, 0); }

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

static void block(LexState *ls) {
  /* block -> chunk */
  FuncState *fs = ls->fs;
  BlockCnt bl;
  enterBlock(fs, &bl, 0);
  chunk(ls);
  DEBUG_ASSERT(bl.breaklist == NO_JUMP);
  leaveBlock(fs);
}

/*
** structure to chain all variables in the left-hand side of an
** assignment
*/
struct LHS_assign {
  struct LHS_assign *prev;
  ExprInfo v; /* variable (global, local, upvalue, or indexed) */
};

/*
** check whether, in an assignment to a local variable, the local variable
** is needed in a previous assignment (to a table). If so, save original
** local value in a safe place and use this safe copy in the previous
** assignment.
*/
static void check_conflict(LexState *ls, struct LHS_assign *lh, ExprInfo *v) {
  FuncState *fs = ls->fs;
  int extra = fs->freereg; /* eventual position to save local variable */
  int conflict = 0;
  for (; lh; lh = lh->prev) {
    if (lh->v.k == VINDEXED) {
      if (lh->v.u.s.info == v->u.s.info) { /* conflict? */
        conflict = 1;
        lh->v.u.s.info = extra; /* previous assignment will use safe copy */
      }
      if (lh->v.u.s.aux == v->u.s.info) { /* conflict? */
        conflict = 1;
        lh->v.u.s.aux = extra; /* previous assignment will use safe copy */
      }
    }
  }
  if (conflict) {
    luaK_codeABC(fs, OP_MOVE, fs->freereg, v->u.s.info, 0); /* make copy */
    luaK_reserveregs(fs, 1);
  }
}

static void assignment(LexState *ls, struct LHS_assign *lh, int nvars) {
  ExprInfo e;
  check_condition(ls, VLOCAL <= lh->v.k && lh->v.k <= VINDEXED, "syntax error");
  if (testNext(ls, ',')) { /* assignment -> `,' primaryexp assignment */
    struct LHS_assign nv;
    nv.prev = lh;
    primaryexp(ls, &nv.v);
    if (nv.v.k == VLOCAL) {
      check_conflict(ls, lh, &nv.v);
    }
    luaY_checklimit(ls->fs, nvars, LUAI_MAX_C_CALLS - ls->L->nestedCCallsNum,
                    "variables in assignment");
    assignment(ls, &nv, nvars + 1);
  } else { /* assignment -> `=' exprList1 */
    int nexps;
    checknext(ls, '=');
    nexps = exprList1(ls, &e);
    if (nexps != nvars) {
      adjust_assign(ls, nvars, nexps, &e);
      if (nexps > nvars) {
        ls->fs->freereg -= nexps - nvars; /* remove extra values */
      }
    } else {
      Codegen_setReturn(ls->fs, &e); /* close last expression */
      luaK_storevar(ls->fs, &lh->v, &e);
      return; /* avoid default */
    }
  }
  exprSetInfo(&e, VNONRELOC, ls->fs->freereg - 1); /* default assignment */
  luaK_storevar(ls->fs, &lh->v, &e);
}

static int cond(LexState *ls) {
  /* cond -> exp */
  ExprInfo v;
  expr(ls, &v); /* read condition */
  if (v.k == VNIL) {
    v.k = VFALSE; /* `falses' are all equal here */
  }
  luaK_goiftrue(ls->fs, &v);
  return v.f;
}

static void breakStmt(LexState *ls) {
  FuncState *fs = ls->fs;
  BlockCnt *bl = fs->bl;
  int upval = 0;
  while (bl && !bl->isbreakable) {
    upval |= bl->upval;
    bl = bl->previous;
  }
  if (!bl) {
    luaX_syntaxerror(ls, "no loop to break");
  }
  if (upval) {
    luaK_codeABC(fs, OP_CLOSE, bl->nactvar, 0, 0);
  }
  luaK_concat(fs, &bl->breaklist, luaK_jump(fs));
}

static void whileStmt(LexState *ls, int line) {
  /* whileStmt -> WHILE cond DO block END */
  FuncState *fs = ls->fs;
  int whileinit;
  int condexit;
  BlockCnt bl;
  luaX_next(ls); /* skip WHILE */
  whileinit = luaK_getlabel(fs);
  condexit = cond(ls);
  enterBlock(fs, &bl, 1);
  checknext(ls, TK_DO);
  block(ls);
  luaK_patchlist(fs, luaK_jump(fs), whileinit);
  check_match(ls, TK_END, TK_WHILE, line);
  leaveBlock(fs);
  luaK_patchtohere(fs, condexit); /* false conditions finish the loop */
}

static void repeatStmt(LexState *ls, int line) {
  /* repeatStmt -> REPEAT block UNTIL cond */
  int condexit;
  FuncState *fs = ls->fs;
  int repeat_init = luaK_getlabel(fs);
  BlockCnt bl1, bl2;
  enterBlock(fs, &bl1, 1); /* loop block */
  enterBlock(fs, &bl2, 0); /* scope block */
  luaX_next(ls);           /* skip REPEAT */
  chunk(ls);
  check_match(ls, TK_UNTIL, TK_REPEAT, line);
  condexit = cond(ls); /* read condition (inside scope block) */
  if (!bl2.upval) {    /* no upvalues? */
    leaveBlock(fs);    /* finish scope */
    luaK_patchlist(ls->fs, condexit, repeat_init); /* close the loop */
  } else {         /* complete semantics when there are upvalues */
    breakStmt(ls); /* if condition then break */
    luaK_patchtohere(ls->fs, condexit);                 /* else... */
    leaveBlock(fs);                                     /* finish scope... */
    luaK_patchlist(ls->fs, luaK_jump(fs), repeat_init); /* and repeat */
  }
  leaveBlock(fs); /* finish loop */
}

static int exp1(LexState *ls) {
  ExprInfo e;
  int k;
  expr(ls, &e);
  k = e.k;
  luaK_exp2nextreg(ls->fs, &e);
  return k;
}

static void forbody(LexState *ls, int base, int line, int nvars, int isnum) {
  /* forbody -> DO block */
  BlockCnt bl;
  FuncState *fs = ls->fs;
  int prep, endfor;
  adjustlocalvars(ls, 3); /* control variables */
  checknext(ls, TK_DO);
  prep = isnum ? luaK_codeAsBx(fs, OP_FORPREP, base, NO_JUMP) : luaK_jump(fs);
  enterBlock(fs, &bl, 0); /* scope for declared variables */
  adjustlocalvars(ls, nvars);
  luaK_reserveregs(fs, nvars);
  block(ls);
  leaveBlock(fs); /* end of scope for declared variables */
  luaK_patchtohere(fs, prep);
  endfor = (isnum) ? luaK_codeAsBx(fs, OP_FORLOOP, base, NO_JUMP)
                   : luaK_codeABC(fs, OP_TFORLOOP, base, 0, nvars);
  luaK_fixline(fs, line); /* pretend that `OP_FOR' starts the loop */
  luaK_patchlist(fs, (isnum ? endfor : luaK_jump(fs)), prep + 1);
}

static void fornum(LexState *ls, StringHeader *varname, int line) {
  /* fornum -> NAME = exp1,exp1[,exp1] forbody */
  FuncState *fs = ls->fs;
  int base = fs->freereg;
  new_localvarliteral(ls, "(for index)", 0);
  new_localvarliteral(ls, "(for limit)", 1);
  new_localvarliteral(ls, "(for step)", 2);
  new_localvar(ls, varname, 3);
  checknext(ls, '=');
  exp1(ls); /* initial value */
  checknext(ls, ',');
  exp1(ls); /* limit */
  if (testNext(ls, ',')) {
    exp1(ls); /* optional step */
  } else {    /* default step = 1 */
    luaK_codeABx(fs, OP_LOADK, fs->freereg, luaK_numberK(fs, 1));
    luaK_reserveregs(fs, 1);
  }
  forbody(ls, base, line, 1, 1);
}

static void forlist(LexState *ls, StringHeader *indexname) {
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
    new_localvar(ls, str_checkname(ls), nvars++);
  }
  checknext(ls, TK_IN);
  line = ls->linenumber;
  adjust_assign(ls, 3, exprList1(ls, &e), &e);
  luaK_checkstack(fs, 3); /* extra space to call generator */
  forbody(ls, base, line, nvars - 3, 0);
}

static void forStmt(LexState *ls, int line) {
  /* forStmt -> FOR (fornum | forlist) END */
  FuncState *fs = ls->fs;
  StringHeader *varname;
  BlockCnt bl;
  enterBlock(fs, &bl, 1);      /* scope for loop and control variables */
  luaX_next(ls);               /* skip `for' */
  varname = str_checkname(ls); /* first variable name */
  switch (ls->t.token) {
  case '=':
    fornum(ls, varname, line);
    break;
  case ',':
  case TK_IN:
    forlist(ls, varname);
    break;
  default:
    luaX_syntaxerror(ls, LUA_QL("=") " or " LUA_QL("in") " expected");
  }
  check_match(ls, TK_END, TK_FOR, line);
  leaveBlock(fs); /* loop scope (`break' jumps to this point) */
}

static int test_then_block(LexState *ls) {
  /* test_then_block -> [IF | ELSEIF] cond THEN block */
  int condexit;
  luaX_next(ls); /* skip IF or ELSEIF */
  condexit = cond(ls);
  checknext(ls, TK_THEN);
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
    luaK_concat(fs, &escapelist, luaK_jump(fs));
    luaK_patchtohere(fs, flist);
    flist = test_then_block(ls); /* ELSEIF cond THEN block */
  }
  if (ls->t.token == TK_ELSE) {
    luaK_concat(fs, &escapelist, luaK_jump(fs));
    luaK_patchtohere(fs, flist);
    luaX_next(ls); /* skip ELSE (after patch, for correct line info) */
    block(ls);     /* `else' part */
  } else {
    luaK_concat(fs, &escapelist, flist);
  }
  luaK_patchtohere(fs, escapelist);
  check_match(ls, TK_END, TK_IF, line);
}

static void localFuncStmt(LexState *ls) {
  ExprInfo v, b;
  FuncState *fs = ls->fs;
  new_localvar(ls, str_checkname(ls), 0);
  exprSetInfo(&v, VLOCAL, fs->freereg);
  luaK_reserveregs(fs, 1);
  adjustlocalvars(ls, 1);
  body(ls, &b, 0, ls->linenumber);
  luaK_storevar(fs, &v, &b);
  /* debug information will only see the variable after this point! */
  getlocvar(fs, fs->nactvar - 1).startPC = fs->pc;
}

static void localStmt(LexState *ls) {
  /* stat -> LOCAL NAME {`,' NAME} [`=' exprList1] */
  int nvars = 0;
  int nexps;
  ExprInfo e;
  do {
    new_localvar(ls, str_checkname(ls), nvars++);
  } while (testNext(ls, ','));
  if (testNext(ls, '=')) {
    nexps = exprList1(ls, &e);
  } else {
    e.k = VVOID;
    nexps = 0;
  }
  adjust_assign(ls, nvars, nexps, &e);
  adjustlocalvars(ls, nvars);
}

static int funcname(LexState *ls, ExprInfo *v) {
  /* funcname -> NAME {field} [`:' NAME] */
  int needself = 0;
  singlevar(ls, v);
  while (ls->t.token == '.') {
    field(ls, v);
  }
  if (ls->t.token == ':') {
    needself = 1;
    field(ls, v);
  }
  return needself;
}

static void funcStmt(LexState *ls, int line) {
  /* funcStmt -> FUNCTION funcname body */
  int needself;
  ExprInfo v, b;
  luaX_next(ls); /* skip FUNCTION */
  needself = funcname(ls, &v);
  body(ls, &b, needself, line);
  luaK_storevar(ls->fs, &v, &b);
  luaK_fixline(ls->fs, line); /* definition `happens' in the first line */
}

static void exprStmt(LexState *ls) {
  /* stat -> func | assignment */
  FuncState *fs = ls->fs;
  struct LHS_assign v;
  primaryexp(ls, &v.v);
  if (v.v.k == VCALL) {             /* stat -> func */
    SETARG_C(getcode(fs, &v.v), 1); /* call stmt uses no results */
  } else {                          /* stat -> assignment */
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
  luaX_next(ls); // skip RETURN

  if (isBlockEnded(ls->t.token) || ls->t.token == ';') {
    // Returns no values.
    luaK_ret(ls->fs, 0, 0);
    return;
  }

  FuncState *fs = ls->fs;
  ExprInfo e;
  int nret = exprList1(ls, &e);

  if (HAS_MULTI_RETURN(e.k)) {
    Codegen_setReturnMulti(fs, &e, LUA_MULTRET);
    if (e.k == VCALL && nret == 1) {
      SET_OPCODE(getcode(fs, &e), OP_TAILCALL);
      DEBUG_ASSERT(GETARG_A(getcode(fs, &e)) == fs->nactvar);
    }
    luaK_ret(fs, fs->nactvar, LUA_MULTRET);
    return;
  }

  int first; // registers with returned values
  if (nret == 1) {
    first = luaK_exp2anyreg(fs, &e);
  } else {
    // Values must go to the stack.
    luaK_exp2nextreg(fs, &e);
    // Returns all active values.
    first = fs->nactvar;
    DEBUG_ASSERT(nret == fs->freereg - first);
  }
  luaK_ret(fs, first, nret);
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
    luaX_next(ls); // skip DO
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
    luaX_next(ls); // skip LOCAL
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
    luaX_next(ls); // skip BREAK
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
    DEBUG_ASSERT(ls->fs->f->maxStackSize >= ls->fs->freereg &&
                 ls->fs->freereg >= ls->fs->nactvar);
    ls->fs->freereg = ls->fs->nactvar; /* free registers */
  }
  leaveLevel(ls);
}
