#include "codegen.h"
#include "debug.h"
#include "gc.h"
#include "lexer.h"
#include "memory.h"
#include "object.h"
#include "opcodes.h"
#include "parser.h"
#include "table.h"

#define HAS_JUMPS(e) ((e)->t != (e)->f)

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
  Codegen_concat(fs, &j, jpc); /* keep them on hold */
  return j;
}

void Codegen_return(FuncState *fs, int first, int nret) {
  Codegen_emitABC(fs, OP_RETURN, first, nret + 1, 0);
}

static int condJump(FuncState *fs, OpCode op, int A, int B, int C) {
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

/// Check whether list has any jump that do not produce a value (or produce an
/// inverted value).
static bool needValue(FuncState *fs, int list) {
  while (list != NO_JUMP) {
    Instruction i = *getJumpControl(fs, list);
    if (GET_OPCODE(i) != OP_TESTSET) {
      return true;
    }
    list = getJump(fs, list);
  }
  return false;
}

static bool patchTestReg(FuncState *fs, int node, int reg) {
  Instruction *i = getJumpControl(fs, node);
  if (GET_OPCODE(*i) != OP_TESTSET) {
    // Cannot patch other instructions.
    return false;
  }
  if (reg != NO_REG && reg != GETARG_B(*i)) {
    SETARG_A(*i, reg);
  } else {
    // No register to put value or register already has the value.
    *i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i));
  }
  return true;
}

static void removeValues(FuncState *fs, int list) {
  while (list != NO_JUMP) {
    patchTestReg(fs, list, NO_REG);
    list = getJump(fs, list);
  }
}

static void patchListAux(FuncState *fs, int list, ptrdiff_t vtarget, int reg,
                         ptrdiff_t dtarget) {
  while (list != NO_JUMP) {
    int next = getJump(fs, list);
    if (patchTestReg(fs, list, reg)) {
      fixJump(fs, list, vtarget);
    } else {
      // Jump to default target.
      fixJump(fs, list, dtarget);
    }
    list = next;
  }
}

static void releaseJpc(FuncState *fs) {
  patchListAux(fs, fs->jpc, (ptrdiff_t)fs->pc, NO_REG, (ptrdiff_t)fs->pc);
  fs->jpc = NO_JUMP;
}

void Codegen_patchList(FuncState *fs, int list, ptrdiff_t target) {
  if (target == (ptrdiff_t)fs->pc) {
    Codegen_patchTo(fs, list);
  } else {
    assert(target < (ptrdiff_t)fs->pc);
    patchListAux(fs, list, target, NO_REG, target);
  }
}

void Codegen_patchTo(FuncState *fs, int list) {
  Codegen_getLabel(fs);
  Codegen_concat(fs, &fs->jpc, list);
}

void Codegen_concat(FuncState *fs, int *l1, int l2) {
  if (l2 == NO_JUMP) {
    return;
  }
  if (*l1 == NO_JUMP) {
    *l1 = l2;
  } else {
    int list = *l1;
    int next = getJump(fs, list);
    // Find last element.
    while (next != NO_JUMP) {
      list = next;
      next = getJump(fs, list);
    }
    fixJump(fs, list, l2);
  }
}

void Codegen_reserveStack(FuncState *fs, int n) {
  uint8_t newStack = fs->freereg + n;
  if (newStack > fs->f->maxStackSize) {
    if (newStack >= MAXSTACK) {
      Lex_throw(fs->ls, "function or expression too complex");
    }
    fs->f->maxStackSize = newStack;
  }
}

void luaK_reserveRegs(FuncState *fs, int n) {
  Codegen_reserveStack(fs, n);
  fs->freereg += n;
}

static void freeReg(FuncState *fs, int reg) {
  if (!ISK(reg) && reg >= fs->nactvar) {
    fs->freereg--;
    assert(reg == fs->freereg);
  }
}

static void freeExpr(FuncState *fs, ExprInfo *e) {
  if (e->k == EXPR_NON_RELOC) {
    freeReg(fs, e->u.nonRelocReg);
  }
}

static size_t addConstant(FuncState *fs, Value *k, Value *v) {
  lua_State *L = fs->L;
  Value *idx = Table_insert(L, fs->h, k);
  Prototype *f = fs->f;

  if (IS_TYPE_NUMBER(idx)) {
    size_t i = NUMBER_VALUE(idx);
    assert(luaO_rawequalObj(&f->constants[i], v));
    return i;
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

size_t Codegen_addNumber(FuncState *fs, double r) {
  Value o;
  SET_NUMBER(&o, r);
  return addConstant(fs, &o, &o);
}

static size_t Codegen_addBool(FuncState *fs, bool b) {
  Value o;
  SET_BOOL(&o, b);
  return addConstant(fs, &o, &o);
}

static size_t addNil(FuncState *fs) {
  Value k, v;
  SET_NIL(&v);
  // Cannot use nil as key. Use table itself to represent nil instead.
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
    luaK_reserveRegs(fs, 1);
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
    freeReg(fs, e->u.indexer.idxReg);
    freeReg(fs, e->u.indexer.tableReg);
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

static void releaseToReg(FuncState *fs, ExprInfo *e, int reg) {
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
    Codegen_emitABx(fs, OP_LOADK, reg, Codegen_addNumber(fs, e->u.numValue));
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

static void releaseToAnyReg(FuncState *fs, ExprInfo *e) {
  if (e->k != EXPR_NON_RELOC) {
    luaK_reserveRegs(fs, 1);
    releaseToReg(fs, e, fs->freereg - 1);
  }
}

static void exprToReg(FuncState *fs, ExprInfo *e, int reg) {
  releaseToReg(fs, e, reg);
  if (e->k == EXPR_JMP) {
    // FIXME(anqur): Suspicious conversion.
    Codegen_concat(fs, &e->t, (int)e->u.jmpPC); /* put this jump in `t' list */
  }
  if (HAS_JUMPS(e)) {
    ptrdiff_t p_f = NO_JUMP; /* position of an eventual LOAD false */
    ptrdiff_t p_t = NO_JUMP; /* position of an eventual LOAD true */
    if (needValue(fs, e->t) || needValue(fs, e->f)) {
      int fj = e->k == EXPR_JMP ? NO_JUMP : Codegen_jump(fs);
      p_f = emitLabel(fs, reg, 0, 1);
      p_t = emitLabel(fs, reg, 1, 0);
      Codegen_patchTo(fs, fj);
    }
    // Position after whole expression.
    ptrdiff_t final = (ptrdiff_t)Codegen_getLabel(fs);
    patchListAux(fs, e->f, final, reg, p_f);
    patchListAux(fs, e->t, final, reg, p_t);
  }
  e->f = NO_JUMP;
  e->t = NO_JUMP;
  e->u.nonRelocReg = reg;
  e->k = EXPR_NON_RELOC;
}

void Codegen_exprToNextReg(FuncState *fs, ExprInfo *e) {
  Codegen_releaseVars(fs, e);
  freeExpr(fs, e);
  luaK_reserveRegs(fs, 1);
  exprToReg(fs, e, fs->freereg - 1);
}

int Codegen_exprToAnyReg(FuncState *fs, ExprInfo *e) {
  Codegen_releaseVars(fs, e);
  if (e->k == EXPR_NON_RELOC) {
    if (!HAS_JUMPS(e)) {
      return e->u.nonRelocReg; /* exp is already in a register */
    }
    if (e->u.nonRelocReg >= fs->nactvar) { /* reg. is not a local? */
      exprToReg(fs, e, e->u.nonRelocReg);  /* put value on it */
      return e->u.nonRelocReg;
    }
  }
  Codegen_exprToNextReg(fs, e);
  assert(e->k == EXPR_NON_RELOC);
  return e->u.nonRelocReg;
}

void Codegen_exprToValue(FuncState *fs, ExprInfo *e) {
  if (HAS_JUMPS(e)) {
    Codegen_exprToAnyReg(fs, e);
  } else {
    Codegen_releaseVars(fs, e);
  }
}

int Codegen_exprToRK(FuncState *fs, ExprInfo *e) {
  Codegen_exprToValue(fs, e);
  switch (e->k) {
  case EXPR_CONST_NUM:
  case EXPR_TRUE:
  case EXPR_FALSE:
  case EXPR_NIL:
    // Constant fits in RK operand?
    if (fs->nk <= MAX_RK_INDEX) {
      e->u.constID = e->k == EXPR_NIL ? addNil(fs)
                     : e->k == EXPR_CONST_NUM
                         ? Codegen_addNumber(fs, e->u.numValue)
                         : Codegen_addBool(fs, e->k == EXPR_TRUE);
      e->k = EXPR_CONST_STR;
      return RK_AS_K(e->u.constID);
    }
    break;
  case EXPR_CONST_STR:
    if (e->u.constID <= MAX_RK_INDEX) {
      // Constant could fit in argC.
      return RK_AS_K(e->u.constID);
    }
    break;
  default:
    break;
  }
  // Not a constant in the right range, put it in a register.
  return Codegen_exprToAnyReg(fs, e);
}

void Codegen_storeVar(FuncState *fs, ExprInfo *var, ExprInfo *ex) {
  switch (var->k) {
  case EXPR_LOCAL:
    freeExpr(fs, ex);
    exprToReg(fs, ex, var->u.localReg);
    return;
  case EXPR_UPVALUE: {
    int e = Codegen_exprToAnyReg(fs, ex);
    // FIXME(anqur): Suspicious conversion,
    Codegen_emitABC(fs, OP_SETUPVAL, e, (int)var->u.upvalueID, 0);
    break;
  }
  case EXPR_GLOBAL: {
    int e = Codegen_exprToAnyReg(fs, ex);
    Codegen_emitABx(fs, OP_SETGLOBAL, e, var->u.globalID);
    break;
  }
  case EXPR_INDEXED: {
    int e = Codegen_exprToRK(fs, ex);
    Codegen_emitABC(fs, OP_SETTABLE, var->u.indexer.tableReg,
                    var->u.indexer.idxReg, e);
    break;
  }
  default:
    assert(false);
  }
  freeExpr(fs, ex);
}

void Codegen_self(FuncState *fs, ExprInfo *e, ExprInfo *key) {
  Codegen_exprToAnyReg(fs, e);
  freeExpr(fs, e);
  int func = fs->freereg;
  luaK_reserveRegs(fs, 2);
  assert(e->k == EXPR_NON_RELOC);
  Codegen_emitABC(fs, OP_SELF, func, e->u.nonRelocReg,
                  Codegen_exprToRK(fs, key));
  freeExpr(fs, key);
  e->u.nonRelocReg = func;
  e->k = EXPR_NON_RELOC;
}

static void invertJump(FuncState *fs, ExprInfo *e) {
  // FIXME(anqur): Suspicious conversion.
  Instruction *pc = getJumpControl(fs, (int)e->u.jmpPC);
  assert(TEST_T_MODE(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
         GET_OPCODE(*pc) != OP_TEST);
  SETARG_A(*pc, !(GETARG_A(*pc)));
}

static int jumpOnCond(FuncState *fs, ExprInfo *e, int cond) {
  if (e->k == EXPR_RELOC) {
    Instruction ie = fs->f->code[e->u.relocatePC];
    if (GET_OPCODE(ie) == OP_NOT) {
      // Remove previous OP_NOT.
      fs->pc--;
      return condJump(fs, OP_TEST, GETARG_B(ie), 0, !cond);
    }
  }
  releaseToAnyReg(fs, e);
  freeExpr(fs, e);
  assert(e->k == EXPR_NON_RELOC);
  return condJump(fs, OP_TESTSET, NO_REG, e->u.nonRelocReg, cond);
}

void Codegen_goIfTrue(FuncState *fs, ExprInfo *e) {
  // PC of last jump.
  int pc = NO_JUMP;
  Codegen_releaseVars(fs, e);
  switch (e->k) {
  case EXPR_CONST_STR:
  case EXPR_CONST_NUM:
  case EXPR_TRUE:
    // Always true, do nothing.
    break;
  case EXPR_JMP:
    invertJump(fs, e);
    // FIXME(anqur): Suspicious conversion.
    pc = (int)e->u.jmpPC;
    break;
  default:
    pc = jumpOnCond(fs, e, 0);
    break;
  }
  // Insert last jump in 'f' list.
  Codegen_concat(fs, &e->f, pc);
  Codegen_patchTo(fs, e->t);
  e->t = NO_JUMP;
}

static void Codegen_goIfFalse(FuncState *fs, ExprInfo *e) {
  // PC of last jump.
  int pc = NO_JUMP;
  Codegen_releaseVars(fs, e);
  switch (e->k) {
  case EXPR_NIL:
  case EXPR_FALSE:
    // Always false, do nothing.
    break;
  case EXPR_JMP:
    // FIXME(anqur): Suspicious conversion.
    pc = (int)e->u.jmpPC;
    break;
  default:
    pc = jumpOnCond(fs, e, 1);
    break;
  }
  // Insert last jump in 't' list.
  Codegen_concat(fs, &e->t, pc);
  Codegen_patchTo(fs, e->f);
  e->f = NO_JUMP;
}

static void emitNot(FuncState *fs, ExprInfo *e) {
  Codegen_releaseVars(fs, e);

  switch (e->k) {
  case EXPR_NIL:
  case EXPR_FALSE:
    e->k = EXPR_TRUE;
    break;
  case EXPR_CONST_STR:
  case EXPR_CONST_NUM:
  case EXPR_TRUE:
    e->k = EXPR_FALSE;
    break;
  case EXPR_JMP:
    invertJump(fs, e);
    break;
  case EXPR_RELOC:
    releaseToAnyReg(fs, e);
    freeExpr(fs, e);
    // FIXME(anqur): Suspicious conversion.
    e->u.relocatePC = Codegen_emitABC(fs, OP_NOT, 0, (int)e->u.relocatePC, 0);
    e->k = EXPR_RELOC;
    break;
  case EXPR_NON_RELOC:
    releaseToAnyReg(fs, e);
    freeExpr(fs, e);
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
  removeValues(fs, e->f);
  removeValues(fs, e->t);
}

void Codegen_indexed(FuncState *fs, ExprInfo *t, ExprInfo *k) {
  t->u.indexer.idxReg = Codegen_exprToRK(fs, k);
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
    r = v1 + v2;
    break;
  case OP_SUB:
    r = v1 - v2;
    break;
  case OP_MUL:
    r = v1 * v2;
    break;
  case OP_DIV:
    if (v2 == 0) {
      // Do not attempt to divide by 0.
      return false;
    }
    r = v1 / v2;
    break;
  case OP_MOD:
    if (v2 == 0) {
      // Do not attempt to divide by 0.
      return 0;
    }
    r = luai_nummod(v1, v2);
    break;
  case OP_POW:
    r = pow(v1, v2);
    break;
  case OP_UNM:
    r = -v1;
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
  int o2 = (op != OP_UNM && op != OP_LEN) ? Codegen_exprToRK(fs, e2) : 0;
  int o1 = Codegen_exprToRK(fs, e1);
  if (o1 > o2) {
    freeExpr(fs, e1);
    freeExpr(fs, e2);
  } else {
    freeExpr(fs, e2);
    freeExpr(fs, e1);
  }
  e1->u.relocatePC = Codegen_emitABC(fs, op, 0, o1, o2);
  e1->k = EXPR_RELOC;
}

static void codeComp(FuncState *fs, OpCode op, int cond, ExprInfo *e1,
                     ExprInfo *e2) {
  int o1 = Codegen_exprToRK(fs, e1);
  int o2 = Codegen_exprToRK(fs, e2);
  freeExpr(fs, e2);
  freeExpr(fs, e1);
  if (cond == 0 && op != OP_EQ) {
    /* exchange args to replace by `<' or `<=' */
    {
      int temp = o1;
      o1 = o2;
      o2 = temp;
    }
    cond = 1;
  }
  e1->u.jmpPC = condJump(fs, op, cond, o1, o2);
  e1->k = EXPR_JMP;
}

void Codegen_prefix(FuncState *fs, OpKind op, ExprInfo *a) {
  ExprInfo b = {.t = NO_JUMP, .f = NO_JUMP, .k = EXPR_CONST_NUM};
  switch (op) {
  case OPR_MINUS:
    if (!isNumeric(a)) {
      Codegen_exprToAnyReg(fs, a); /* cannot operate on non-numeric constants */
    }
    emitArith(fs, OP_UNM, a, &b);
    break;
  case OPR_NOT:
    emitNot(fs, a);
    break;
  case OPR_LEN:
    Codegen_exprToAnyReg(fs, a); /* cannot operate on constants */
    emitArith(fs, OP_LEN, a, &b);
    break;
  default:
    assert(false);
  }
}

void Codegen_infix(FuncState *fs, OpKind op, ExprInfo *v) {
  switch (op) {
  case OPR_AND: {
    Codegen_goIfTrue(fs, v);
    break;
  }
  case OPR_OR: {
    Codegen_goIfFalse(fs, v);
    break;
  }
  case OPR_CONCAT: {
    Codegen_exprToNextReg(fs, v); /* operand must be on the `stack' */
    break;
  }
  case OPR_ADD:
  case OPR_SUB:
  case OPR_MUL:
  case OPR_DIV:
  case OPR_MOD:
  case OPR_POW: {
    if (!isNumeric(v)) {
      Codegen_exprToRK(fs, v);
    }
    break;
  }
  default: {
    Codegen_exprToRK(fs, v);
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
    Codegen_concat(fs, &e2->f, e1->f);
    *e1 = *e2;
    break;
  case OPR_OR:
    // List must be closed.
    assert(e1->f == NO_JUMP);
    Codegen_releaseVars(fs, e2);
    Codegen_concat(fs, &e2->t, e1->t);
    *e1 = *e2;
    break;
  case OPR_CONCAT:
    Codegen_exprToValue(fs, e2);
    if (e2->k == EXPR_RELOC &&
        GET_OPCODE(fs->f->code[e2->u.relocatePC]) == OP_CONCAT) {
      assert(e1->k == EXPR_NON_RELOC);
      assert(e1->u.nonRelocReg == GETARG_B(fs->f->code[e2->u.relocatePC]) - 1);
      freeExpr(fs, e1);
      SETARG_B(fs->f->code[e2->u.relocatePC], e1->u.nonRelocReg);
      e1->k = EXPR_RELOC;
      e1->u.relocatePC = e2->u.relocatePC;
    } else {
      Codegen_exprToNextReg(fs, e2); /* operand must be on the 'stack' */
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
    codeComp(fs, OP_EQ, 1, e1, e2);
    break;
  case OPR_NE:
    codeComp(fs, OP_EQ, 0, e1, e2);
    break;
  case OPR_LT:
    codeComp(fs, OP_LT, 1, e1, e2);
    break;
  case OPR_LE:
    codeComp(fs, OP_LE, 1, e1, e2);
    break;
  case OPR_GT:
    codeComp(fs, OP_LT, 0, e1, e2);
    break;
  case OPR_GE:
    codeComp(fs, OP_LE, 0, e1, e2);
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
  // PC will change.
  releaseJpc(fs);
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
  // Free registers with list values.
  fs->freereg = base + 1;
}
