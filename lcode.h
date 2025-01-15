/* Code generator for Lua. */

#pragma once

#include "llex.h"
#include "lobject.h"
#include "lopcodes.h"
#include "parser.h"

/*
** Marks the end of a patch list. It is an invalid value both as an absolute
** address, and as a list link (would link an element to itself).
*/
#define NO_JUMP (-1)

/*
** grep "ORDER OPR" if you change these enums
*/
typedef enum BinOpr {
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
  OPR_NOBINOPR
} BinOpr;

typedef enum UnOpr { OPR_MINUS, OPR_NOT, OPR_LEN, OPR_NOUNOPR } UnOpr;

#define getcode(fs, e) ((fs)->f->code[(e)->u.s.info])

#define luaK_codeAsBx(fs, o, A, sBx) luaK_codeABx(fs, o, A, (sBx) + MAXARG_sBx)

LUAI_FUNC int luaK_codeABx(FuncState *fs, OpCode o, int A, unsigned int Bx);
LUAI_FUNC int luaK_codeABC(FuncState *fs, OpCode o, int A, int B, int C);
LUAI_FUNC void luaK_fixline(FuncState *fs, int line);
LUAI_FUNC void luaK_nil(FuncState *fs, int from, int n);
LUAI_FUNC void luaK_reserveregs(FuncState *fs, int n);
LUAI_FUNC void luaK_checkstack(FuncState *fs, int n);
LUAI_FUNC int luaK_stringK(FuncState *fs, TString *s);
LUAI_FUNC int luaK_numberK(FuncState *fs, lua_Number r);
LUAI_FUNC void luaK_dischargevars(FuncState *fs, ExprInfo *e);
LUAI_FUNC int luaK_exp2anyreg(FuncState *fs, ExprInfo *e);
LUAI_FUNC void luaK_exp2nextreg(FuncState *fs, ExprInfo *e);
LUAI_FUNC void luaK_exp2val(FuncState *fs, ExprInfo *e);
LUAI_FUNC int luaK_exp2RK(FuncState *fs, ExprInfo *e);
LUAI_FUNC void luaK_self(FuncState *fs, ExprInfo *e, ExprInfo *key);
LUAI_FUNC void luaK_indexed(FuncState *fs, ExprInfo *t, ExprInfo *k);
LUAI_FUNC void luaK_goiftrue(FuncState *fs, ExprInfo *e);
LUAI_FUNC void luaK_storevar(FuncState *fs, ExprInfo *var, ExprInfo *e);
LUAI_FUNC void Codegen_setReturnMulti(FuncState *fs, ExprInfo *e,
                                      int resultsNum);
LUAI_FUNC void Codegen_setReturn(FuncState *fs, ExprInfo *e);
LUAI_FUNC int luaK_jump(FuncState *fs);
LUAI_FUNC void luaK_ret(FuncState *fs, int first, int nret);
LUAI_FUNC void luaK_patchlist(FuncState *fs, int list, int target);
LUAI_FUNC void luaK_patchtohere(FuncState *fs, int list);
LUAI_FUNC void luaK_concat(FuncState *fs, int *l1, int l2);
LUAI_FUNC int luaK_getlabel(FuncState *fs);
LUAI_FUNC void luaK_prefix(FuncState *fs, UnOpr op, ExprInfo *v);
LUAI_FUNC void luaK_infix(FuncState *fs, BinOpr op, ExprInfo *v);
LUAI_FUNC void luaK_posfix(FuncState *fs, BinOpr op, ExprInfo *v1,
                           ExprInfo *v2);
LUAI_FUNC void luaK_setlist(FuncState *fs, int base, int nelems, int tostore);
