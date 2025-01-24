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
  return e->k == EXPR_CONST_NUM && e->t == NO_JUMP && e->f == NO_JUMP;
}

void Codegen_emitNil(FuncState *fs, int from, int n) {
  // No jumps to current position?
  if ((ptrdiff_t)fs->pc > fs->lasttarget) {
    // Function start?
    if (fs->pc == 0) {
      if (from >= fs->nactvar) {
        // Positions are already clean.
        return;
      }
    } else {
      Instruction *prev = &fs->f->code[fs->pc - 1];
      if (GET_OPCODE(*prev) == OP_LOADNIL) {
        int pfrom = GETARG_A(*prev);
        int pto = GETARG_B(*prev);

        // Can connect both?
        if (pfrom <= from && from <= pto + 1) {
          if (from + n - 1 > pto) {
            SETARG_B(*prev, from + n - 1);
          }
          return;
        }
      }
    }
  }

  // Slow path, no optimization.
  Codegen_emitABC(fs, OP_LOADNIL, from, from + n - 1, 0);
}

int Codegen_jump(FuncState *fs) {
  int jpc = fs->jpc; /* save list of jumps to here */
  fs->jpc = NO_JUMP;
  int j = luaK_codeAsBx(fs, OP_JMP, 0, NO_JUMP);
  luaK_concat(fs, &j, jpc); /* keep them on hold */
  return j;
}

void Codegen_return(FuncState *fs, int first, int nret) {
  Codegen_emitABC(fs, OP_RETURN, first, nret + 1, 0);
}

static int condjump(FuncState *fs, OpCode op, int A, int B, int C) {
  Codegen_emitABC(fs, op, A, B, C);
  return Codegen_jump(fs);
}

static void fixJump(FuncState *fs, size_t pc, ptrdiff_t dest) {
  Instruction *jmp = &fs->f->code[pc];
  ptrdiff_t offset = dest - ((ptrdiff_t)pc + 1);
  assert(dest != NO_JUMP);
  if (labs(offset) > MAXARG_sBx) {
    Lex_throw(fs->ls, "control structure too long");
  }
  SETARG_sBx(*jmp, offset);
}

/// Returns current PC and marks it as a jump target (to avoid wrong
/// optimizations with consecutive instructions not in the same basic block).
size_t Codegen_getLabel(FuncState *fs) {
  fs->lasttarget = (ptrdiff_t)fs->pc;
  return fs->pc;
}

static int getJump(FuncState *fs, int pc) {
  int offset = GETARG_sBx(fs->f->code[pc]);
  if (offset == NO_JUMP) {
    // Point to itself represents the end of list.
    return NO_JUMP;
  }
  // Turn offset into absolute position.
  return (pc + 1) + offset;
}

static Instruction *getJumpControl(FuncState *fs, int pc) {
  Instruction *i = &fs->f->code[pc];
  if (pc >= 1 && TEST_T_MODE(GET_OPCODE(*(i - 1)))) {
    return i - 1;
  }
  return i;
}

/*
** check whether list has any jump that do not produce a value
** (or produce an inverted value)
*/
static int need_value(FuncState *fs, int list) {
  for (; list != NO_JUMP; list = getJump(fs, list)) {
    Instruction i = *getJumpControl(fs, list);
    if (GET_OPCODE(i) != OP_TESTSET) {
      return 1;
    }
  }
  return 0; /* not found */
}

static int patchtestreg(FuncState *fs, int node, int reg) {
  Instruction *i = getJumpControl(fs, node);
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
  for (; list != NO_JUMP; list = getJump(fs, list)) {
    patchtestreg(fs, list, NO_REG);
  }
}

static void patchlistaux(FuncState *fs, int list, ptrdiff_t vtarget, int reg,
                         ptrdiff_t dtarget) {
  while (list != NO_JUMP) {
    int next = getJump(fs, list);
    if (patchtestreg(fs, list, reg)) {
      fixJump(fs, list, vtarget);
    } else {
      fixJump(fs, list, dtarget); /* jump to default target */
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
  Codegen_getLabel(fs);
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
    while ((next = getJump(fs, list)) != NO_JUMP) { /* find last element */
      list = next;
    }
    fixJump(fs, list, l2);
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
  if (e->k == EXPR_NON_RELOC) {
    freereg(fs, e->u.nonRelocReg);
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
  if (e->k == EXPR_CALL) {
    // Expression is an open function call?
    SETARG_C(fs->f->code[e->u.callPC], resultsNum + 1);
  } else if (e->k == EXPR_VARARG_CALL) {
    const size_t pc = e->u.varargCallPC;
    SETARG_B(fs->f->code[pc], resultsNum + 1);
    SETARG_A(fs->f->code[pc], fs->freereg);
    luaK_reserveregs(fs, 1);
  }
}

void Codegen_setReturn(FuncState *fs, ExprInfo *e) {
  if (e->k == EXPR_CALL) { /* expression is an open function call? */
    e->k = EXPR_NON_RELOC;
    e->u.nonRelocReg = GETARG_A(fs->f->code[e->u.callPC]);
  } else if (e->k == EXPR_VARARG_CALL) {
    SETARG_B(fs->f->code[e->u.varargCallPC], 2);
    // Can relocate its simple result.
    e->k = EXPR_RELOC;
    e->u.relocatePC = e->u.varargCallPC;
  }
}

void Codegen_releaseVars(FuncState *fs, ExprInfo *e) {
  switch (e->k) {
  case EXPR_LOCAL:
    e->u.nonRelocReg = e->u.localReg;
    e->k = EXPR_NON_RELOC;
    break;
  case EXPR_UPVALUE:
    // FIXME(anqur): Suspicious conversion.
    e->u.relocatePC =
        Codegen_emitABC(fs, OP_GETUPVAL, 0, (int)e->u.upvalueID, 0);
    e->k = EXPR_RELOC;
    break;
  case EXPR_GLOBAL:
    e->u.relocatePC = Codegen_emitABx(fs, OP_GETGLOBAL, 0, e->u.globalID);
    e->k = EXPR_RELOC;
    break;
  case EXPR_INDEXED:
    freereg(fs, e->u.indexer.idxReg);
    freereg(fs, e->u.indexer.tableReg);
    e->u.relocatePC = Codegen_emitABC(fs, OP_GETTABLE, 0, e->u.indexer.tableReg,
                                      e->u.indexer.idxReg);
    e->k = EXPR_RELOC;
    break;
  case EXPR_VARARG_CALL:
  case EXPR_CALL:
    Codegen_setReturn(fs, e);
    break;
  default:
    // There is one value available (somewhere).
    break;
  }
}

static ptrdiff_t emitLabel(FuncState *fs, int A, int b, int jump) {
  // Those instructions may be jump targets.
  Codegen_getLabel(fs);
  return (ptrdiff_t)Codegen_emitABC(fs, OP_LOADBOOL, A, b, jump);
}

static void discharge2reg(FuncState *fs, ExprInfo *e, int reg) {
  Codegen_releaseVars(fs, e);
  switch (e->k) {
  case EXPR_NIL:
    Codegen_emitNil(fs, reg, 1);
    break;
  case EXPR_FALSE:
  case EXPR_TRUE:
    Codegen_emitABC(fs, OP_LOADBOOL, reg, e->k == EXPR_TRUE, 0);
    break;
  case EXPR_CONST_STR:
    Codegen_emitABx(fs, OP_LOADK, reg, e->u.constID);
    break;
  case EXPR_CONST_NUM:
    Codegen_emitABx(fs, OP_LOADK, reg, luaK_numberK(fs, e->u.numValue));
    break;
  case EXPR_RELOC:
    Instruction *pc = &fs->f->code[e->u.relocatePC];
    SETARG_A(*pc, reg);
    break;
  case EXPR_NON_RELOC:
    if (reg != e->u.nonRelocReg) {
      Codegen_emitABC(fs, OP_MOVE, reg, e->u.nonRelocReg, 0);
    }
    break;
  default:
    // Nothing to do here.
    assert(e->k == EXPR_VOID || e->k == EXPR_JMP);
    return;
  }
  e->u.nonRelocReg = reg;
  e->k = EXPR_NON_RELOC;
}

static void discharge2anyreg(FuncState *fs, ExprInfo *e) {
  if (e->k != EXPR_NON_RELOC) {
    luaK_reserveregs(fs, 1);
    discharge2reg(fs, e, fs->freereg - 1);
  }
}

static void exp2reg(FuncState *fs, ExprInfo *e, int reg) {
  discharge2reg(fs, e, reg);
  if (e->k == EXPR_JMP) {
    // FIXME(anqur): Suspicious conversion.
    luaK_concat(fs, &e->t, (int)e->u.jmpPC); /* put this jump in `t' list */
  }
  if (hasjumps(e)) {
    ptrdiff_t p_f = NO_JUMP; /* position of an eventual LOAD false */
    ptrdiff_t p_t = NO_JUMP; /* position of an eventual LOAD true */
    if (need_value(fs, e->t) || need_value(fs, e->f)) {
      int fj = (e->k == EXPR_JMP) ? NO_JUMP : Codegen_jump(fs);
      p_f = emitLabel(fs, reg, 0, 1);
      p_t = emitLabel(fs, reg, 1, 0);
      luaK_patchtohere(fs, fj);
    }
    // Position after whole expression.
    ptrdiff_t final = (ptrdiff_t)Codegen_getLabel(fs);
    patchlistaux(fs, e->f, final, reg, p_f);
    patchlistaux(fs, e->t, final, reg, p_t);
  }
  e->f = NO_JUMP;
  e->t = NO_JUMP;
  e->u.nonRelocReg = reg;
  e->k = EXPR_NON_RELOC;
}

void luaK_exp2nextreg(FuncState *fs, ExprInfo *e) {
  Codegen_releaseVars(fs, e);
  freeexp(fs, e);
  luaK_reserveregs(fs, 1);
  exp2reg(fs, e, fs->freereg - 1);
}

int luaK_exp2anyreg(FuncState *fs, ExprInfo *e) {
  Codegen_releaseVars(fs, e);
  if (e->k == EXPR_NON_RELOC) {
    if (!hasjumps(e)) {
      return e->u.nonRelocReg; /* exp is already in a register */
    }
    if (e->u.nonRelocReg >= fs->nactvar) { /* reg. is not a local? */
      exp2reg(fs, e, e->u.nonRelocReg);    /* put value on it */
      return e->u.nonRelocReg;
    }
  }
  luaK_exp2nextreg(fs, e);
  assert(e->k == EXPR_NON_RELOC);
  return e->u.nonRelocReg;
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
  case EXPR_CONST_NUM:
  case EXPR_TRUE:
  case EXPR_FALSE:
  case EXPR_NIL:
    // Constant fits in RK operand?
    if (fs->nk <= MAXINDEXRK) {
      e->u.constID = e->k == EXPR_NIL         ? nilK(fs)
                     : e->k == EXPR_CONST_NUM ? luaK_numberK(fs, e->u.numValue)
                                              : boolK(fs, (e->k == EXPR_TRUE));
      e->k = EXPR_CONST_STR;
      return RKASK(e->u.constID);
    }
    break;
  case EXPR_CONST_STR:
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
  case EXPR_LOCAL: {
    freeexp(fs, ex);
    exp2reg(fs, ex, var->u.localReg);
    return;
  }
  case EXPR_UPVALUE: {
    int e = luaK_exp2anyreg(fs, ex);
    // FIXME(anqur): Suspicious conversion,
    Codegen_emitABC(fs, OP_SETUPVAL, e, (int)var->u.upvalueID, 0);
    break;
  }
  case EXPR_GLOBAL: {
    int e = luaK_exp2anyreg(fs, ex);
    Codegen_emitABx(fs, OP_SETGLOBAL, e, var->u.globalID);
    break;
  }
  case EXPR_INDEXED: {
    int e = luaK_exp2RK(fs, ex);
    Codegen_emitABC(fs, OP_SETTABLE, var->u.indexer.tableReg,
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
  luaK_exp2anyreg(fs, e);
  freeexp(fs, e);
  int func = fs->freereg;
  luaK_reserveregs(fs, 2);
  assert(e->k == EXPR_NON_RELOC);
  Codegen_emitABC(fs, OP_SELF, func, e->u.nonRelocReg, luaK_exp2RK(fs, key));
  freeexp(fs, key);
  e->u.nonRelocReg = func;
  e->k = EXPR_NON_RELOC;
}

static void invertjump(FuncState *fs, ExprInfo *e) {
  // FIXME(anqur): Suspicious conversion.
  Instruction *pc = getJumpControl(fs, (int)e->u.jmpPC);
  assert(TEST_T_MODE(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
         GET_OPCODE(*pc) != OP_TEST);
  SETARG_A(*pc, !(GETARG_A(*pc)));
}

static int jumponcond(FuncState *fs, ExprInfo *e, int cond) {
  if (e->k == EXPR_RELOC) {
    Instruction ie = fs->f->code[e->u.relocatePC];
    if (GET_OPCODE(ie) == OP_NOT) {
      fs->pc--; /* remove previous OP_NOT */
      return condjump(fs, OP_TEST, GETARG_B(ie), 0, !cond);
    }
    /* else go through */
  }
  discharge2anyreg(fs, e);
  freeexp(fs, e);
  assert(e->k == EXPR_NON_RELOC);
  return condjump(fs, OP_TESTSET, NO_REG, e->u.nonRelocReg, cond);
}

void luaK_goiftrue(FuncState *fs, ExprInfo *e) {
  int pc; /* pc of last jump */
  Codegen_releaseVars(fs, e);
  switch (e->k) {
  case EXPR_CONST_STR:
  case EXPR_CONST_NUM:
  case EXPR_TRUE:
    pc = NO_JUMP; /* always true; do nothing */
    break;
  case EXPR_JMP:
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
  case EXPR_NIL:
  case EXPR_FALSE: {
    pc = NO_JUMP; /* always false; do nothing */
    break;
  }
  case EXPR_JMP: {
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
  case EXPR_NIL:
  case EXPR_FALSE: {
    e->k = EXPR_TRUE;
    break;
  }
  case EXPR_CONST_STR:
  case EXPR_CONST_NUM:
  case EXPR_TRUE: {
    e->k = EXPR_FALSE;
    break;
  }
  case EXPR_JMP: {
    invertjump(fs, e);
    break;
  }
  case EXPR_RELOC:
    discharge2anyreg(fs, e);
    freeexp(fs, e);
    // FIXME(anqur): Suspicious conversion.
    e->u.relocatePC = Codegen_emitABC(fs, OP_NOT, 0, (int)e->u.relocatePC, 0);
    e->k = EXPR_RELOC;
    break;
  case EXPR_NON_RELOC:
    discharge2anyreg(fs, e);
    freeexp(fs, e);
    e->u.relocatePC = Codegen_emitABC(fs, OP_NOT, 0, e->u.nonRelocReg, 0);
    e->k = EXPR_RELOC;
    break;
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
  t->k = EXPR_INDEXED;
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
  e1->u.relocatePC = Codegen_emitABC(fs, op, 0, o1, o2);
  e1->k = EXPR_RELOC;
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
  e1->k = EXPR_JMP;
}

void Codegen_prefix(FuncState *fs, OpKind op, ExprInfo *a) {
  ExprInfo b = {.t = NO_JUMP, .f = NO_JUMP, .k = EXPR_CONST_NUM};
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

void Codegen_infix(FuncState *fs, OpKind op, ExprInfo *v) {
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

void Codegen_suffix(FuncState *fs, OpKind op, ExprInfo *e1, ExprInfo *e2) {
  switch (op) {
  case OPR_AND:
    // List must be closed.
    assert(e1->t == NO_JUMP);
    Codegen_releaseVars(fs, e2);
    luaK_concat(fs, &e2->f, e1->f);
    *e1 = *e2;
    break;
  case OPR_OR:
    // List must be closed.
    assert(e1->f == NO_JUMP);
    Codegen_releaseVars(fs, e2);
    luaK_concat(fs, &e2->t, e1->t);
    *e1 = *e2;
    break;
  case OPR_CONCAT:
    luaK_exp2val(fs, e2);
    if (e2->k == EXPR_RELOC &&
        GET_OPCODE(fs->f->code[e2->u.relocatePC]) == OP_CONCAT) {
      assert(e1->k == EXPR_NON_RELOC);
      assert(e1->u.nonRelocReg == GETARG_B(fs->f->code[e2->u.relocatePC]) - 1);
      freeexp(fs, e1);
      SETARG_B(fs->f->code[e2->u.relocatePC], e1->u.nonRelocReg);
      e1->k = EXPR_RELOC;
      e1->u.relocatePC = e2->u.relocatePC;
    } else {
      luaK_exp2nextreg(fs, e2); /* operand must be on the 'stack' */
      emitArith(fs, OP_CONCAT, e1, e2);
    }
    break;
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
    assert(false);
  }
}

void Codegen_fixLine(FuncState *fs, int line) {
  fs->f->lineInfo[fs->pc - 1] = line;
}

static size_t emitCode(FuncState *fs, Instruction i, int line) {
  Prototype *f = fs->f;
  // `pc` will change.
  dischargejpc(fs);
  Mem_growVec(fs->L, f->code, fs->pc, f->codeSize, Instruction, SAFE_INT_MAX,
              "code size overflow");
  f->code[fs->pc] = i;
  Mem_growVec(fs->L, f->lineInfo, fs->pc, f->lineInfoSize, int, SAFE_INT_MAX,
              "line info size overflow");
  f->lineInfo[fs->pc] = line;
  return fs->pc++;
}

size_t Codegen_emitABC(FuncState *fs, OpCode o, int a, int b, int c) {
  assert(GET_OP_MODE(o) == FORMAT_A_B_C);
  assert(GET_B_MODE(o) != OP_ARG_NOT_USED || b == 0);
  assert(GET_C_MODE(o) != OP_ARG_NOT_USED || c == 0);
  return emitCode(fs, CREATE_ABC(o, a, b, c), fs->ls->lastline);
}

size_t Codegen_emitABx(FuncState *fs, OpCode o, int A, unsigned int Bx) {
  assert(GET_OP_MODE(o) == FORMAT_A_Bx || GET_OP_MODE(o) == FORMAT_A_sBx);
  assert(GET_C_MODE(o) == OP_ARG_NOT_USED);
  return emitCode(fs, CREATE_ABx(o, A, Bx), fs->ls->lastline);
}

void luaK_setlist(FuncState *fs, int base, int nelems, int tostore) {
  int c = (nelems - 1) / LFIELDS_PER_FLUSH + 1;
  int b = (tostore == LUA_MULTRET) ? 0 : tostore;
  assert(tostore != 0);
  if (c <= MAXARG_C) {
    Codegen_emitABC(fs, OP_SETLIST, base, b, c);
  } else {
    Codegen_emitABC(fs, OP_SETLIST, base, b, 0);
    emitCode(fs, (Instruction)c, fs->ls->lastline);
  }
  fs->freereg = base + 1; /* free registers with list values */
}
