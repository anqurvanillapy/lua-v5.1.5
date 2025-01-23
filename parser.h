/* Lua Parser. */

#pragma once

#include "buffer.h"
#include "limits.h"
#include "object.h"

typedef enum ExprKind {
  // No extra info.
  VVOID,
  VNIL,
  VTRUE,
  VFALSE,

  // Use constID.
  VK,
  // Use numValue.
  VKNUM,
  // Use localReg.
  VLOCAL,
  // Use upvalueID.
  VUPVAL,
  // Use globalID.
  VGLOBAL,
  // Use indexer.
  VINDEXED,
  // Use jmpPC.
  VJMP,
  /* info = instruction pc */
  VRELOCABLE,
  /* info = result register */
  VNONRELOC,
  // Use callPC.
  VCALL,
  /* info = instruction pc */
  VVARARG
} ExprKind;

typedef struct ExprIndexer {
  // Register of the target table.
  int tableReg;
  // Register of the index argument.
  int idxReg;
} ExprIndexer;

typedef union ExprVariant {
  size_t constID;
  double numValue;
  int localReg;
  size_t upvalueID;
  size_t globalID;
  ExprIndexer indexer;
  size_t jmpPC;
  size_t callPC;

  struct {
    int info;
    int aux;
  } s;
} ExprVariant;

typedef struct ExprInfo {
  ExprKind k;
  ExprVariant u;
  int t; /* patch list of `exit when true' */
  int f; /* patch list of `exit when false' */
} ExprInfo;

typedef struct UpvalueInfo {
  ExprKind k;
  int info;
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
  UpvalueInfo upvalues[LUAI_MAX_UPVALUES]; /* upvalues */
  int actvar[LUAI_MAX_VARS];               /* declared-variable stack */
} FuncState;

LUAI_FUNC Prototype *luaY_parser(lua_State *L, ZIO *z, Mbuffer *buff,
                                 const char *name);
