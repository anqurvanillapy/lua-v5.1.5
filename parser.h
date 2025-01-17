/* Lua Parser. */

#pragma once

#include "limits.h"
#include "lzio.h"
#include "object.h"

typedef enum ExprKind {
  VVOID, /* no value */
  VNIL,
  VTRUE,
  VFALSE,
  VK, /* info = index of constant in `k' */
  VKNUM,
  VLOCAL,     /* info = local register */
  VUPVAL,     /* info = index of upvalue in `upvalues' */
  VGLOBAL,    /* info = index of table; aux = index of global name in `k' */
  VINDEXED,   /* info = table register; aux = index register (or `k') */
  VJMP,       /* info = instruction pc */
  VRELOCABLE, /* info = instruction pc */
  VNONRELOC,  /* info = result register */
  VCALL,      /* info = instruction pc */
  VVARARG     /* info = instruction pc */
} ExprKind;

typedef struct ExprInfo {
  ExprKind k;
  union {
    struct {
      int info;
      int aux;
    } s;
    double value;
  } u;
  int t; /* patch list of `exit when true' */
  int f; /* patch list of `exit when false' */
} ExprInfo;

typedef struct UpvalueInfo {
  ExprKind k;
  int info;
} UpvalueInfo;

struct BlockCnt; /* defined in lparser.c */

/* state needed to generate code for a given function */
typedef struct FuncState {
  Prototype *f;           /* current function header */
  Table *h;               /* table to find (and reuse) elements in `k' */
  struct FuncState *prev; /* enclosing function */
  struct LexState *ls;    /* lexical state */
  struct lua_State *L;    /* copy of the Lua state */
  struct BlockCnt *bl;    /* chain of current blocks */
  int pc;                 /* next position to code (equivalent to `ncode') */
  int lasttarget;         /* `pc' of last `jump target' */
  int jpc;                /* list of pending jumps to `pc' */
  int freereg;            /* first free register */
  int nk;                 /* number of elements in `k' */
  int np;                 /* number of elements in `p' */
  short nlocvars;         /* number of elements in `locVars' */
  uint8_t nactvar;        /* number of active local variables */
  UpvalueInfo upvalues[LUAI_MAX_UPVALUES]; /* upvalues */
  unsigned short actvar[LUAI_MAX_VARS];    /* declared-variable stack */
} FuncState;

LUAI_FUNC Prototype *luaY_parser(lua_State *L, ZIO *z, Mbuffer *buff,
                                 const char *name);
