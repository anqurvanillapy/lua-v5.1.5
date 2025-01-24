#pragma once

#include "lexer.h"
#include "object.h"
#include "opcodes.h"
#include "parser.h"

/*
** Marks the end of a patch list. It is an invalid value both as an absolute
** address, and as a list link (would link an element to itself).
*/
#define NO_JUMP (-1)

typedef enum OpKind {
  OPR_NONE,

  // Unary.

  OPR_MINUS,
  OPR_NOT,
  OPR_LEN,

  // Binary.

  OPR_ADD,
  OPR_SUB,
  OPR_MUL,
  OPR_DIV,
  OPR_MOD,
  OPR_POW,
  OPR_CONCAT,
  OPR_NE,
  OPR_EQ,
  OPR_LT,
  OPR_LE,
  OPR_GT,
  OPR_GE,
  OPR_AND,
  OPR_OR,
} OpKind;

#define luaK_codeAsBx(fs, o, A, sBx)                                           \
  Codegen_emitABx(fs, o, A, (sBx) + MAXARG_sBx)

LUAI_FUNC size_t Codegen_emitABx(FuncState *fs, OpCode o, int A,
                                 unsigned int Bx);
LUAI_FUNC size_t Codegen_emitABC(FuncState *fs, OpCode o, int a, int b, int c);
LUAI_FUNC void Codegen_fixLine(FuncState *fs, int line);
LUAI_FUNC void Codegen_emitNil(FuncState *fs, int from, int n);
LUAI_FUNC void luaK_reserveRegs(FuncState *fs, int n);
LUAI_FUNC void Codegen_reserveStack(FuncState *fs, int n);
LUAI_FUNC size_t Codegen_addString(FuncState *fs, String *s);
LUAI_FUNC size_t Codegen_addNumber(FuncState *fs, double r);
LUAI_FUNC void Codegen_releaseVars(FuncState *fs, ExprInfo *e);
LUAI_FUNC int Codegen_exprToAnyReg(FuncState *fs, ExprInfo *e);
LUAI_FUNC void Codegen_exprToNextReg(FuncState *fs, ExprInfo *e);
LUAI_FUNC void Codegen_exprToValue(FuncState *fs, ExprInfo *e);
LUAI_FUNC int Codegen_exprToRK(FuncState *fs, ExprInfo *e);
LUAI_FUNC void Codegen_self(FuncState *fs, ExprInfo *e, ExprInfo *key);
LUAI_FUNC void Codegen_indexed(FuncState *fs, ExprInfo *t, ExprInfo *k);
LUAI_FUNC void Codegen_goIfTrue(FuncState *fs, ExprInfo *e);
LUAI_FUNC void Codegen_storeVar(FuncState *fs, ExprInfo *var, ExprInfo *ex);
LUAI_FUNC void Codegen_setReturnMulti(FuncState *fs, ExprInfo *e,
                                      int resultsNum);
LUAI_FUNC void Codegen_setReturn(FuncState *fs, ExprInfo *e);
LUAI_FUNC int Codegen_jump(FuncState *fs);
LUAI_FUNC void Codegen_return(FuncState *fs, int first, int nret);
LUAI_FUNC void Codegen_patchList(FuncState *fs, int list, ptrdiff_t target);
LUAI_FUNC void Codegen_patchTo(FuncState *fs, int list);
LUAI_FUNC void Codegen_concat(FuncState *fs, int *l1, int l2);
LUAI_FUNC size_t Codegen_getLabel(FuncState *fs);
LUAI_FUNC void Codegen_prefix(FuncState *fs, OpKind op, ExprInfo *a);
LUAI_FUNC void Codegen_infix(FuncState *fs, OpKind op, ExprInfo *v);
LUAI_FUNC void Codegen_suffix(FuncState *fs, OpKind op, ExprInfo *e1,
                              ExprInfo *e2);
LUAI_FUNC void luaK_setlist(FuncState *fs, int base, int nelems, int tostore);
