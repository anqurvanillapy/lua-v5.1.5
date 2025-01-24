/* Lua Parser. */

#pragma once

#include "buffer.h"
#include "limits.h"
#include "object.h"

typedef enum ExprKind {
  // No extra info.

  EXPR_VOID,
  EXPR_NIL,
  EXPR_TRUE,
  EXPR_FALSE,

  // Use constID.
  EXPR_CONST_STR,
  // Use numValue.
  EXPR_CONST_NUM,
  // Use localReg.
  EXPR_LOCAL,
  // Use upvalueID.
  EXPR_UPVALUE,
  // Use globalID.
  EXPR_GLOBAL,
  // Use indexer.
  EXPR_INDEXED,
  // Use jmpPC.
  EXPR_JMP,
  // Use relocatePC.
  EXPR_RELOC,
  // Use nonRelocReg. Non-relocatable result register.
  EXPR_NON_RELOC,
  // Use callPC.
  EXPR_CALL,
  // Use varargCallPC.
  EXPR_VARARG_CALL,
} ExprKind;

typedef struct Indexer {
  // Register of the target table.
  int tableReg;
  // Register of the index argument.
  int idxReg;
} Indexer;

typedef union ExprVariant {
  size_t constID;
  double numValue;
  int localReg;
  size_t upvalueID;
  size_t globalID;
  Indexer indexer;
  size_t jmpPC;
  size_t relocatePC;
  int nonRelocReg;
  size_t callPC;
  size_t varargCallPC;
} ExprVariant;

typedef struct ExprInfo {
  ExprKind k;
  ExprVariant u;
  int t; /* patch list of `exit when true' */
  int f; /* patch list of `exit when false' */
} ExprInfo;

typedef union UpvalueVariant {
  int localReg;
  size_t upvalueID;
} UpvalueVariant;

typedef struct UpvalueInfo {
  ExprKind k;
  UpvalueVariant v;
} UpvalueInfo;

struct Block;

/* state needed to generate code for a given function */
typedef struct FuncState {
  Prototype *f;           /* current function header */
  Table *h;               /* table to find (and reuse) elements in `k' */
  struct FuncState *prev; /* enclosing function */
  struct LexState *ls;    /* lexical state */
  struct lua_State *L;    /* copy of the Lua state */
  struct Block *bl;       /* chain of current blocks */
  size_t pc;              /* next position to code (equivalent to `ncode') */
  ptrdiff_t lasttarget;   /* `pc' of last `jump target' */
  int jpc;                /* list of pending jumps to `pc' */
  int freereg;            /* first free register */
  size_t nk;              /* number of elements in `k' */
  size_t np;              /* number of elements in `p' */
  size_t nlocvars;        /* number of elements in `locVars' */
  uint8_t nactvar;        /* number of active local variables */
  UpvalueInfo upvalues[LUAI_MAX_UPVALUES];
  int actvar[LUAI_MAX_VARS]; /* declared-variable stack */
} FuncState;

LUAI_FUNC Prototype *luaY_parser(lua_State *L, ZIO *z, StringBuilder *buff,
                                 const char *name);
