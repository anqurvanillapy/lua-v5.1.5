#include <stdlib.h>

#include "lua.h"

#include "codegen.h"
#include "debug.h"
#include "gc.h"
#include "lexer.h"
#include "memory.h"
#include "object.h"
#include "opcodes.h"
#include "parser.h"
#include "table.h"

#define hasjumps(e) ((e)->t != (e)->f)

static int isNumeric(ExprInfo *e) {
  return e->k == VKNUM && e->t == NO_JUMP && e->f == NO_JUMP;
}

void luaK_nil(FuncState *fs, int from, int n) {
  Instruction *previous;
  if ((ptrdiff_t)fs->pc > fs->lasttarget) { /* no jumps to current position? */
    if (fs->pc == 0) {                      /* function start? */
      if (from >= fs->nactvar) {
        return; /* positions are already clean */
      }
    } else {
      previous = &fs->f->code[fs->pc - 1];
      if (GET_OPCODE(*previous) == OP_LOADNIL) {
        int pfrom = GETARG_A(*previous);
        int pto = GETARG_B(*previous);
        if (pfrom <= from && from <= pto + 1) { /* can connect both? */
          if (from + n - 1 > pto) {
            SETARG_B(*previous, from + n - 1);
          }
          return;
        }
      }
    }
  }
  luaK_codeABC(fs, OP_LOADNIL, from, from + n - 1,
               0); /* else no optimization */
}

int luaK_jump(FuncState *fs) {
  int jpc = fs->jpc; /* save list of jumps to here */
  fs->jpc = NO_JUMP;
  int j = luaK_codeAsBx(fs, OP_JMP, 0, NO_JUMP);
  luaK_concat(fs, &j, jpc); /* keep them on hold */
  return j;
}

void luaK_ret(FuncState *fs, int first, int nret) {
  luaK_codeABC(fs, OP_RETURN, first, nret + 1, 0);
}

static int condjump(FuncState *fs, OpCode op, int A, int B, int C) {
  luaK_codeABC(fs, op, A, B, C);
  return luaK_jump(fs);
}

static void fixjump(FuncState *fs, size_t pc, ptrdiff_t dest) {
  Instruction *jmp = &fs->f->code[pc];
  ptrdiff_t offset = dest - ((ptrdiff_t)pc + 1);
  assert(dest != NO_JUMP);
  if (labs(offset) > MAXARG_sBx) {
    Lex_throw(fs->ls, "control structure too long");
  }
  SETARG_sBx(*jmp, offset);
}

/*
** returns current `pc' and marks it as a jump target (to avoid wrong
** optimizations with consecutive instructions not in the same basic block).
*/
size_t luaK_getlabel(FuncState *fs) {
  fs->lasttarget = (ptrdiff_t)fs->pc;
  return fs->pc;
}

static int getjump(FuncState *fs, int pc) {
  int offset = GETARG_sBx(fs->f->code[pc]);
  if (offset == NO_JUMP) { /* point to itself represents end of list */
    return NO_JUMP;        /* end of list */
  } else {
    return (pc + 1) + offset; /* turn offset into absolute position */
  }
}

static Instruction *getjumpcontrol(FuncState *fs, int pc) {
  Instruction *pi = &fs->f->code[pc];
  if (pc >= 1 && testTMode(GET_OPCODE(*(pi - 1)))) {
    return pi - 1;
  }
  return pi;
}

/*
** check whether list has any jump that do not produce a value
** (or produce an inverted value)
*/
static int need_value(FuncState *fs, int list) {
  for (; list != NO_JUMP; list = getjump(fs, list)) {
    Instruction i = *getjumpcontrol(fs, list);
    if (GET_OPCODE(i) != OP_TESTSET) {
      return 1;
    }
  }
  return 0; /* not found */
}

static int patchtestreg(FuncState *fs, int node, int reg) {
  Instruction *i = getjumpcontrol(fs, node);
  if (GET_OPCODE(*i) != OP_TESTSET) {
    return 0; /* cannot patch other instructions */
  }
  if (reg != NO_REG && reg != GETARG_B(*i)) {
    SETARG_A(*i, reg);
  } else { /* no register to put value or register already has the value */
    *i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i));
  }

  return 1;
}

static void removevalues(FuncState *fs, int list) {
  for (; list != NO_JUMP; list = getjump(fs, list)) {
    patchtestreg(fs, list, NO_REG);
  }
}

static void patchlistaux(FuncState *fs, int list, ptrdiff_t vtarget, int reg,
                         ptrdiff_t dtarget) {
  while (list != NO_JUMP) {
    int next = getjump(fs, list);
    if (patchtestreg(fs, list, reg)) {
      fixjump(fs, list, vtarget);
    } else {
      fixjump(fs, list, dtarget); /* jump to default target */
    }
    list = next;
  }
}

static void dischargejpc(FuncState *fs) {
  patchlistaux(fs, fs->jpc, (ptrdiff_t)fs->pc, NO_REG, (ptrdiff_t)fs->pc);
  fs->jpc = NO_JUMP;
}

void luaK_patchlist(FuncState *fs, int list, ptrdiff_t target) {
  if (target == (ptrdiff_t)fs->pc) {
    luaK_patchtohere(fs, list);
  } else {
    assert(target < (ptrdiff_t)fs->pc);
    patchlistaux(fs, list, target, NO_REG, target);
  }
}

void luaK_patchtohere(FuncState *fs, int list) {
  luaK_getlabel(fs);
  luaK_concat(fs, &fs->jpc, list);
}

void luaK_concat(FuncState *fs, int *l1, int l2) {
  if (l2 == NO_JUMP) {
    return;
  }
  if (*l1 == NO_JUMP) {
    *l1 = l2;
  } else {
    int list = *l1;
    int next;
    while ((next = getjump(fs, list)) != NO_JUMP) { /* find last element */
      list = next;
    }
    fixjump(fs, list, l2);
  }
}

void luaK_checkstack(FuncState *fs, int n) {
  int newstack = fs->freereg + n;
  if (newstack > fs->f->maxStackSize) {
    if (newstack >= MAXSTACK) {
      Lex_throw(fs->ls, "function or expression too complex");
    }
    fs->f->maxStackSize = (uint8_t)newstack;
  }
}

void luaK_reserveregs(FuncState *fs, int n) {
  luaK_checkstack(fs, n);
  fs->freereg += n;
}

static void freereg(FuncState *fs, int reg) {
  if (!ISK(reg) && reg >= fs->nactvar) {
    fs->freereg--;
    assert(reg == fs->freereg);
  }
}

static void freeexp(FuncState *fs, ExprInfo *e) {
  if (e->k == VNONRELOC) {
    freereg(fs, e->u.info);
  }
}

static size_t addConstant(FuncState *fs, Value *k, Value *v) {
  lua_State *L = fs->L;
  Value *idx = Table_insert(L, fs->h, k);
  Prototype *f = fs->f;

  if (IS_TYPE_NUMBER(idx)) {
    assert(luaO_rawequalObj(&f->constants[(int)NUMBER_VALUE(idx)], v));
    return (int)NUMBER_VALUE(idx);
  }

  // Constant not found, create a new entry.
  size_t oldSize = f->constantsSize;
  SET_NUMBER(idx, (double)fs->nk);
  Mem_growVec(L, f->constants, fs->nk, f->constantsSize, Value, MAXARG_Bx,
              "constant table overflow");
  while (oldSize < f->constantsSize) {
    SET_NIL(&f->constants[oldSize++]);
  }
  SET_OBJECT(L, &f->constants[fs->nk], v);
  luaC_barrier(L, f, v);
  return fs->nk++;
}

size_t Codegen_addString(FuncState *fs, String *s) {
  Value o;
  SET_STRING(fs->L, &o, s);
  return addConstant(fs, &o, &o);
}

size_t luaK_numberK(FuncState *fs, double r) {
  Value o;
  SET_NUMBER(&o, r);
  return addConstant(fs, &o, &o);
}

static size_t boolK(FuncState *fs, int b) {
  Value o;
  SET_BOOL(&o, b);
  return addConstant(fs, &o, &o);
}

static size_t nilK(FuncState *fs) {
  Value k, v;
  SET_NIL(&v);
  /* cannot use nil as key; instead use table itself to represent nil */
  SET_TABLE(fs->L, &k, fs->h);
  return addConstant(fs, &k, &v);
}

void Codegen_setReturnMulti(FuncState *fs, ExprInfo *e, int resultsNum) {
  if (e->k == VCALL) {
    // Expression is an open function call?
    SETARG_C(fs->f->code[e->u.callPC], resultsNum + 1);
  } else if (e->k == VVARARG) {
    SETARG_B(fs->f->code[e->u.info], resultsNum + 1);
    SETARG_A(fs->f->code[e->u.info], fs->freereg);
    luaK_reserveregs(fs, 1);
  }
}

void Codegen_setReturn(FuncState *fs, ExprInfo *e) {
  if (e->k == VCALL) { /* expression is an open function call? */
    e->k = VNONRELOC;
    e->u.info = GETARG_A(fs->f->code[e->u.callPC]);
  } else if (e->k == VVARARG) {
    SETARG_B(fs->f->code[e->u.info], 2);
    e->k = VRELOCABLE; /* can relocate its simple result */
  }
}

void Codegen_releaseVars(FuncState *fs, ExprInfo *e) {
  switch (e->k) {
  case VLOCAL:
    e->k = VNONRELOC;
    break;
  case VUPVAL:
    // FIXME(anqur): Suspicious conversion.
    e->u.info = luaK_codeABC(fs, OP_GETUPVAL, 0, (int)e->u.upvalueID, 0);
    e->k = VRELOCABLE;
    break;
  case VGLOBAL:
    e->u.info = luaK_codeABx(fs, OP_GETGLOBAL, 0, e->u.globalID);
    e->k = VRELOCABLE;
    break;
  case VINDEXED:
    freereg(fs, e->u.indexer.idxReg);
    freereg(fs, e->u.indexer.tableReg);
    e->u.info = luaK_codeABC(fs, OP_GETTABLE, 0, e->u.indexer.tableReg,
                             e->u.indexer.idxReg);
    e->k = VRELOCABLE;
    break;
  case VVARARG:
  case VCALL:
    Codegen_setReturn(fs, e);
    break;
  default:
    // There is one value available (somewhere).
    break;
  }
}

static ptrdiff_t emitLabel(FuncState *fs, int A, int b, int jump) {
  // Those instructions may be jump targets.
  luaK_getlabel(fs);
  return (ptrdiff_t)luaK_codeABC(fs, OP_LOADBOOL, A, b, jump);
}

static void discharge2reg(FuncState *fs, ExprInfo *e, int reg) {
  Codegen_releaseVars(fs, e);
  switch (e->k) {
  case VNIL:
    luaK_nil(fs, reg, 1);
    break;
  case VFALSE:
  case VTRUE:
    luaK_codeABC(fs, OP_LOADBOOL, reg, e->k == VTRUE, 0);
    break;
  case VK:
    luaK_codeABx(fs, OP_LOADK, reg, e->u.constID);
    break;
  case VKNUM:
    luaK_codeABx(fs, OP_LOADK, reg, luaK_numberK(fs, e->u.numValue));
    break;
  case VRELOCABLE: {
    Instruction *pc = &fs->f->code[e->u.info];
    SETARG_A(*pc, reg);
    break;
  }
  case VNONRELOC:
    if (reg != e->u.info) {
      luaK_codeABC(fs, OP_MOVE, reg, e->u.info, 0);
    }
    break;
  default:
    // Nothing to do here.
    assert(e->k == VVOID || e->k == VJMP);
    return;
  }
  e->u.info = reg;
  e->k = VNONRELOC;
}

static void discharge2anyreg(FuncState *fs, ExprInfo *e) {
  if (e->k != VNONRELOC) {
    luaK_reserveregs(fs, 1);
    discharge2reg(fs, e, fs->freereg - 1);
  }
}

static void exp2reg(FuncState *fs, ExprInfo *e, int reg) {
  discharge2reg(fs, e, reg);
  if (e->k == VJMP) {
    // FIXME(anqur): Suspicious conversion.
    luaK_concat(fs, &e->t, (int)e->u.jmpPC); /* put this jump in `t' list */
  }
  if (hasjumps(e)) {
    ptrdiff_t p_f = NO_JUMP; /* position of an eventual LOAD false */
    ptrdiff_t p_t = NO_JUMP; /* position of an eventual LOAD true */
    if (need_value(fs, e->t) || need_value(fs, e->f)) {
      int fj = (e->k == VJMP) ? NO_JUMP : luaK_jump(fs);
      p_f = emitLabel(fs, reg, 0, 1);
      p_t = emitLabel(fs, reg, 1, 0);
      luaK_patchtohere(fs, fj);
    }
    // Position after whole expression.
    ptrdiff_t final = (ptrdiff_t)luaK_getlabel(fs);
    patchlistaux(fs, e->f, final, reg, p_f);
    patchlistaux(fs, e->t, final, reg, p_t);
  }
  e->f = e->t = NO_JUMP;
  e->u.info = reg;
  e->k = VNONRELOC;
}

void luaK_exp2nextreg(FuncState *fs, ExprInfo *e) {
  Codegen_releaseVars(fs, e);
  freeexp(fs, e);
  luaK_reserveregs(fs, 1);
  exp2reg(fs, e, fs->freereg - 1);
}

int luaK_exp2anyreg(FuncState *fs, ExprInfo *e) {
  Codegen_releaseVars(fs, e);
  if (e->k == VNONRELOC) {
    if (!hasjumps(e)) {
      return e->u.info; /* exp is already in a register */
    }
    if (e->u.info >= fs->nactvar) { /* reg. is not a local? */
      exp2reg(fs, e, e->u.info);    /* put value on it */
      return e->u.info;
    }
  }
  luaK_exp2nextreg(fs, e); /* default */
  return e->u.info;
}

void luaK_exp2val(FuncState *fs, ExprInfo *e) {
  if (hasjumps(e)) {
    luaK_exp2anyreg(fs, e);
  } else {
    Codegen_releaseVars(fs, e);
  }
}

int luaK_exp2RK(FuncState *fs, ExprInfo *e) {
  luaK_exp2val(fs, e);
  switch (e->k) {
  case VKNUM:
  case VTRUE:
  case VFALSE:
  case VNIL:
    // Constant fits in RK operand?
    if (fs->nk <= MAXINDEXRK) {
      e->u.constID = e->k == VNIL    ? nilK(fs)
                     : e->k == VKNUM ? luaK_numberK(fs, e->u.numValue)
                                     : boolK(fs, (e->k == VTRUE));
      e->k = VK;
      return RKASK(e->u.constID);
    }
    break;
  case VK:
    if (e->u.constID <= MAXINDEXRK) {
      // Constant could fit in argC.
      return RKASK(e->u.constID);
    }
    break;
  default:
    break;
  }
  /* not a constant in the right range: put it in a register */
  return luaK_exp2anyreg(fs, e);
}

void luaK_storevar(FuncState *fs, ExprInfo *var, ExprInfo *ex) {
  switch (var->k) {
  case VLOCAL: {
    freeexp(fs, ex);
    exp2reg(fs, ex, var->u.localReg);
    return;
  }
  case VUPVAL: {
    int e = luaK_exp2anyreg(fs, ex);
    // FIXME(anqur): Suspicious conversion,
    luaK_codeABC(fs, OP_SETUPVAL, e, (int)var->u.upvalueID, 0);
    break;
  }
  case VGLOBAL: {
    int e = luaK_exp2anyreg(fs, ex);
    luaK_codeABx(fs, OP_SETGLOBAL, e, var->u.globalID);
    break;
  }
  case VINDEXED: {
    int e = luaK_exp2RK(fs, ex);
    luaK_codeABC(fs, OP_SETTABLE, var->u.indexer.tableReg,
                 var->u.indexer.idxReg, e);
    break;
  }
  default: {
    assert(false);
  }
  }
  freeexp(fs, ex);
}

void luaK_self(FuncState *fs, ExprInfo *e, ExprInfo *key) {
  int func;
  luaK_exp2anyreg(fs, e);
  freeexp(fs, e);
  func = fs->freereg;
  luaK_reserveregs(fs, 2);
  luaK_codeABC(fs, OP_SELF, func, e->u.info, luaK_exp2RK(fs, key));
  freeexp(fs, key);
  e->u.info = func;
  e->k = VNONRELOC;
}

static void invertjump(FuncState *fs, ExprInfo *e) {
  // FIXME(anqur): Suspicious conversion.
  Instruction *pc = getjumpcontrol(fs, (int)e->u.jmpPC);
  assert(testTMode(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
         GET_OPCODE(*pc) != OP_TEST);
  SETARG_A(*pc, !(GETARG_A(*pc)));
}

static int jumponcond(FuncState *fs, ExprInfo *e, int cond) {
  if (e->k == VRELOCABLE) {
    Instruction ie = fs->f->code[e->u.info];
    if (GET_OPCODE(ie) == OP_NOT) {
      fs->pc--; /* remove previous OP_NOT */
      return condjump(fs, OP_TEST, GETARG_B(ie), 0, !cond);
    }
    /* else go through */
  }
  discharge2anyreg(fs, e);
  freeexp(fs, e);
  return condjump(fs, OP_TESTSET, NO_REG, e->u.info, cond);
}

void luaK_goiftrue(FuncState *fs, ExprInfo *e) {
  int pc; /* pc of last jump */
  Codegen_releaseVars(fs, e);
  switch (e->k) {
  case VK:
  case VKNUM:
  case VTRUE:
    pc = NO_JUMP; /* always true; do nothing */
    break;
  case VJMP:
    invertjump(fs, e);
    // FIXME(anqur): Suspicious conversion.
    pc = (int)e->u.jmpPC;
    break;
  default:
    pc = jumponcond(fs, e, 0);
    break;
  }
  luaK_concat(fs, &e->f, pc); /* insert last jump in `f' list */
  luaK_patchtohere(fs, e->t);
  e->t = NO_JUMP;
}

static void luaK_goiffalse(FuncState *fs, ExprInfo *e) {
  int pc; /* pc of last jump */
  Codegen_releaseVars(fs, e);
  switch (e->k) {
  case VNIL:
  case VFALSE: {
    pc = NO_JUMP; /* always false; do nothing */
    break;
  }
  case VJMP: {
    // FIXME(anqur): Suspicious conversion.
    pc = (int)e->u.jmpPC;
    break;
  }
  default: {
    pc = jumponcond(fs, e, 1);
    break;
  }
  }
  luaK_concat(fs, &e->t, pc); /* insert last jump in `t' list */
  luaK_patchtohere(fs, e->f);
  e->f = NO_JUMP;
}

static void emitNot(FuncState *fs, ExprInfo *e) {
  Codegen_releaseVars(fs, e);

  switch (e->k) {
  case VNIL:
  case VFALSE: {
    e->k = VTRUE;
    break;
  }
  case VK:
  case VKNUM:
  case VTRUE: {
    e->k = VFALSE;
    break;
  }
  case VJMP: {
    invertjump(fs, e);
    break;
  }
  case VRELOCABLE:
  case VNONRELOC: {
    discharge2anyreg(fs, e);
    freeexp(fs, e);
    e->u.info = luaK_codeABC(fs, OP_NOT, 0, e->u.info, 0);
    e->k = VRELOCABLE;
    break;
  }
  default:
    assert(false);
  }

  {
    int falseList = e->f;
    e->f = e->t;
    e->t = falseList;
  }
  removevalues(fs, e->f);
  removevalues(fs, e->t);
}

void luaK_indexed(FuncState *fs, ExprInfo *t, ExprInfo *k) {
  t->u.indexer.idxReg = luaK_exp2RK(fs, k);
  t->k = VINDEXED;
}

static bool constantFolding(OpCode op, ExprInfo *e1, ExprInfo *e2) {
  if (!isNumeric(e1) || !isNumeric(e2)) {
    return false;
  }
  double r;
  double v1 = e1->u.numValue;
  double v2 = e2->u.numValue;
  switch (op) {
  case OP_ADD:
    r = luai_numadd(v1, v2);
    break;
  case OP_SUB:
    r = luai_numsub(v1, v2);
    break;
  case OP_MUL:
    r = luai_nummul(v1, v2);
    break;
  case OP_DIV:
    if (v2 == 0) {
      // Do not attempt to divide by 0.
      return false;
    }
    r = luai_numdiv(v1, v2);
    break;
  case OP_MOD:
    if (v2 == 0) {
      // Do not attempt to divide by 0.
      return 0;
    }
    r = luai_nummod(v1, v2);
    break;
  case OP_POW:
    r = luai_numpow(v1, v2);
    break;
  case OP_UNM:
    r = luai_numunm(v1);
    break;
  case OP_LEN:
    return false;
  default:
    assert(false);
  }
  if (isnan(r)) {
    // Do not attempt to produce NaNs.
    return false;
  }
  e1->u.numValue = r;
  return true;
}

static void emitArith(FuncState *fs, OpCode op, ExprInfo *e1, ExprInfo *e2) {
  if (constantFolding(op, e1, e2)) {
    return;
  }
  int o2 = (op != OP_UNM && op != OP_LEN) ? luaK_exp2RK(fs, e2) : 0;
  int o1 = luaK_exp2RK(fs, e1);
  if (o1 > o2) {
    freeexp(fs, e1);
    freeexp(fs, e2);
  } else {
    freeexp(fs, e2);
    freeexp(fs, e1);
  }
  e1->u.info = luaK_codeABC(fs, op, 0, o1, o2);
  e1->k = VRELOCABLE;
}

static void codecomp(FuncState *fs, OpCode op, int cond, ExprInfo *e1,
                     ExprInfo *e2) {
  int o1 = luaK_exp2RK(fs, e1);
  int o2 = luaK_exp2RK(fs, e2);
  freeexp(fs, e2);
  freeexp(fs, e1);
  if (cond == 0 && op != OP_EQ) {
    /* exchange args to replace by `<' or `<=' */
    {
      int temp = o1;
      o1 = o2;
      o2 = temp;
    }
    cond = 1;
  }
  e1->u.jmpPC = condjump(fs, op, cond, o1, o2);
  e1->k = VJMP;
}

void Codegen_prefix(FuncState *fs, OpKind op, ExprInfo *a) {
  ExprInfo b = {.t = NO_JUMP, .f = NO_JUMP, .k = VKNUM};
  switch (op) {
  case OPR_MINUS:
    if (!isNumeric(a)) {
      luaK_exp2anyreg(fs, a); /* cannot operate on non-numeric constants */
    }
    emitArith(fs, OP_UNM, a, &b);
    break;
  case OPR_NOT:
    emitNot(fs, a);
    break;
  case OPR_LEN:
    luaK_exp2anyreg(fs, a); /* cannot operate on constants */
    emitArith(fs, OP_LEN, a, &b);
    break;
  default:
    assert(false);
  }
}

void luaK_infix(FuncState *fs, OpKind op, ExprInfo *v) {
  switch (op) {
  case OPR_AND: {
    luaK_goiftrue(fs, v);
    break;
  }
  case OPR_OR: {
    luaK_goiffalse(fs, v);
    break;
  }
  case OPR_CONCAT: {
    luaK_exp2nextreg(fs, v); /* operand must be on the `stack' */
    break;
  }
  case OPR_ADD:
  case OPR_SUB:
  case OPR_MUL:
  case OPR_DIV:
  case OPR_MOD:
  case OPR_POW: {
    if (!isNumeric(v)) {
      luaK_exp2RK(fs, v);
    }
    break;
  }
  default: {
    luaK_exp2RK(fs, v);
    break;
  }
  }
}

void luaK_posfix(FuncState *fs, OpKind op, ExprInfo *e1, ExprInfo *e2) {
  switch (op) {
  case OPR_AND: {
    assert(e1->t == NO_JUMP); /* list must be closed */
    Codegen_releaseVars(fs, e2);
    luaK_concat(fs, &e2->f, e1->f);
    *e1 = *e2;
    break;
  }
  case OPR_OR: {
    assert(e1->f == NO_JUMP); /* list must be closed */
    Codegen_releaseVars(fs, e2);
    luaK_concat(fs, &e2->t, e1->t);
    *e1 = *e2;
    break;
  }
  case OPR_CONCAT: {
    luaK_exp2val(fs, e2);
    if (e2->k == VRELOCABLE &&
        GET_OPCODE(fs->f->code[e2->u.info]) == OP_CONCAT) {
      assert(e1->u.info == GETARG_B(fs->f->code[e2->u.info]) - 1);
      freeexp(fs, e1);
      SETARG_B(fs->f->code[e2->u.info], e1->u.info);
      e1->k = VRELOCABLE;
      e1->u.info = e2->u.info;
    } else {
      luaK_exp2nextreg(fs, e2); /* operand must be on the 'stack' */
      emitArith(fs, OP_CONCAT, e1, e2);
    }
    break;
  }
  case OPR_ADD:
    emitArith(fs, OP_ADD, e1, e2);
    break;
  case OPR_SUB:
    emitArith(fs, OP_SUB, e1, e2);
    break;
  case OPR_MUL:
    emitArith(fs, OP_MUL, e1, e2);
    break;
  case OPR_DIV:
    emitArith(fs, OP_DIV, e1, e2);
    break;
  case OPR_MOD:
    emitArith(fs, OP_MOD, e1, e2);
    break;
  case OPR_POW:
    emitArith(fs, OP_POW, e1, e2);
    break;
  case OPR_EQ:
    codecomp(fs, OP_EQ, 1, e1, e2);
    break;
  case OPR_NE:
    codecomp(fs, OP_EQ, 0, e1, e2);
    break;
  case OPR_LT:
    codecomp(fs, OP_LT, 1, e1, e2);
    break;
  case OPR_LE:
    codecomp(fs, OP_LE, 1, e1, e2);
    break;
  case OPR_GT:
    codecomp(fs, OP_LT, 0, e1, e2);
    break;
  case OPR_GE:
    codecomp(fs, OP_LE, 0, e1, e2);
    break;
  default:
    assert(0);
  }
}

void luaK_fixline(FuncState *fs, int line) {
  fs->f->lineInfo[fs->pc - 1] = line;
}

static size_t luaK_code(FuncState *fs, Instruction i, int line) {
  Prototype *f = fs->f;
  dischargejpc(fs); /* `pc' will change */
  /* put new instruction in code array */
  Mem_growVec(fs->L, f->code, fs->pc, f->codeSize, Instruction, SAFE_INT_MAX,
              "code size overflow");
  f->code[fs->pc] = i;
  /* save corresponding line information */
  Mem_growVec(fs->L, f->lineInfo, fs->pc, f->lineInfoSize, int, SAFE_INT_MAX,
              "code size overflow");
  f->lineInfo[fs->pc] = line;
  return fs->pc++;
}

size_t luaK_codeABC(FuncState *fs, OpCode o, int a, int b, int c) {
  assert(getOpMode(o) == iABC);
  assert(getBMode(o) != OpArgN || b == 0);
  assert(getCMode(o) != OpArgN || c == 0);
  return luaK_code(fs, CREATE_ABC(o, a, b, c), fs->ls->lastline);
}

size_t luaK_codeABx(FuncState *fs, OpCode o, int a, unsigned int bc) {
  assert(getOpMode(o) == iABx || getOpMode(o) == iAsBx);
  assert(getCMode(o) == OpArgN);
  return luaK_code(fs, CREATE_ABx(o, a, bc), fs->ls->lastline);
}

void luaK_setlist(FuncState *fs, int base, int nelems, int tostore) {
  int c = (nelems - 1) / LFIELDS_PER_FLUSH + 1;
  int b = (tostore == LUA_MULTRET) ? 0 : tostore;
  assert(tostore != 0);
  if (c <= MAXARG_C) {
    luaK_codeABC(fs, OP_SETLIST, base, b, c);
  } else {
    luaK_codeABC(fs, OP_SETLIST, base, b, 0);
    luaK_code(fs, cast(Instruction, c), fs->ls->lastline);
  }
  fs->freereg = base + 1; /* free registers with list values */
}
