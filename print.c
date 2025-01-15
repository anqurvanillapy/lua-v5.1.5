/* Print bytecodes. */

#include <ctype.h>
#include <stdio.h>

#define luac_c

#include "ldebug.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lundump.h"

#define PrintFunction luaU_print

#define Sizeof(x) ((int)sizeof(x))
#define VOID(p) ((const void *)(p))

static void PrintString(const TString *ts) {
  const char *s = GET_STR(ts);
  size_t i, n = ts->tsv.len;
  putchar('"');
  for (i = 0; i < n; i++) {
    int c = s[i];
    switch (c) {
    case '"':
      printf("\\\"");
      break;
    case '\\':
      printf("\\\\");
      break;
    case '\a':
      printf("\\a");
      break;
    case '\b':
      printf("\\b");
      break;
    case '\f':
      printf("\\f");
      break;
    case '\n':
      printf("\\n");
      break;
    case '\r':
      printf("\\r");
      break;
    case '\t':
      printf("\\t");
      break;
    case '\v':
      printf("\\v");
      break;
    default:
      if (isprint((unsigned char)c)) {
        putchar(c);
      } else {
        printf("\\%03u", (unsigned char)c);
      }
    }
  }
  putchar('"');
}

static void PrintConstant(const Prototype *f, int i) {
  const TaggedValue *o = &f->k[i];
  switch (GET_TYPE(o)) {
  case LUA_TYPE_NIL:
    printf("nil");
    break;
  case LUA_TYPE_BOOLEAN:
    printf(BOOL_VALUE(o) ? "true" : "false");
    break;
  case LUA_TYPE_NUMBER:
    printf(LUA_NUMBER_FMT, NUMBER_VALUE(o));
    break;
  case LUA_TYPE_STRING:
    PrintString(RAW_STRING_VALUE(o));
    break;
  default: /* cannot happen */
    printf("? type=%d", GET_TYPE(o));
    break;
  }
}

static void PrintCode(const Prototype *f) {
  const Instruction *code = f->code;
  int pc, n = f->codeSize;
  for (pc = 0; pc < n; pc++) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    int bx = GETARG_Bx(i);
    int sbx = GETARG_sBx(i);
    int line = getline(f, pc);
    printf("\t%d\t", pc + 1);
    if (line > 0) {
      printf("[%d]\t", line);
    } else {
      printf("[-]\t");
    }
    printf("%-9s\t", luaP_opnames[o]);
    switch (getOpMode(o)) {
    case iABC:
      printf("%d", a);
      if (getBMode(o) != OpArgN) {
        printf(" %d", ISK(b) ? (-1 - INDEXK(b)) : b);
      }
      if (getCMode(o) != OpArgN) {
        printf(" %d", ISK(c) ? (-1 - INDEXK(c)) : c);
      }
      break;
    case iABx:
      if (getBMode(o) == OpArgK) {
        printf("%d %d", a, -1 - bx);
      } else {
        printf("%d %d", a, bx);
      }
      break;
    case iAsBx:
      if (o == OP_JMP) {
        printf("%d", sbx);
      } else {
        printf("%d %d", a, sbx);
      }
      break;
    }
    switch (o) {
    case OP_LOADK:
      printf("\t; ");
      PrintConstant(f, bx);
      break;
    case OP_GETUPVAL:
    case OP_SETUPVAL:
      printf("\t; %s", (f->upvaluesSize > 0) ? GET_STR(f->upvalues[b]) : "-");
      break;
    case OP_GETGLOBAL:
    case OP_SETGLOBAL:
      printf("\t; %s", GET_STR_VALUE(&f->k[bx]));
      break;
    case OP_GETTABLE:
    case OP_SELF:
      if (ISK(c)) {
        printf("\t; ");
        PrintConstant(f, INDEXK(c));
      }
      break;
    case OP_SETTABLE:
    case OP_ADD:
    case OP_SUB:
    case OP_MUL:
    case OP_DIV:
    case OP_POW:
    case OP_EQ:
    case OP_LT:
    case OP_LE:
      if (ISK(b) || ISK(c)) {
        printf("\t; ");
        if (ISK(b)) {
          PrintConstant(f, INDEXK(b));
        } else {
          printf("-");
        }
        printf(" ");
        if (ISK(c)) {
          PrintConstant(f, INDEXK(c));
        } else {
          printf("-");
        }
      }
      break;
    case OP_JMP:
    case OP_FORLOOP:
    case OP_FORPREP:
      printf("\t; to %d", sbx + pc + 2);
      break;
    case OP_CLOSURE:
      printf("\t; %p", VOID(f->inners[bx]));
      break;
    case OP_SETLIST:
      if (c == 0) {
        printf("\t; %d", (int)code[++pc]);
      } else {
        printf("\t; %d", c);
      }
      break;
    default:
      break;
    }
    printf("\n");
  }
}

#define SS(x) (x == 1) ? "" : "s"
#define S(x) x, SS(x)

static void PrintHeader(const Prototype *f) {
  const char *s = GET_STR(f->source);
  if (*s == '@' || *s == '=') {
    s++;
  } else if (*s == LUA_SIGNATURE[0]) {
    s = "(bstring)";
  } else {
    s = "(string)";
  }
  printf("\n%s <%s:%d,%d> (%d instruction%s, %d bytes at %p)\n",
         (f->lineDefined == 0) ? "main" : "function", s, f->lineDefined,
         f->lineDefinedLast, S(f->codeSize), f->codeSize * Sizeof(Instruction),
         VOID(f));
  printf("%d%s param%s, %d slot%s, %d upvalue%s, ", f->paramsNum,
         f->varargMode ? "+" : "", SS(f->paramsNum), S(f->maxStackSize),
         S(f->upvaluesNum));
  printf("%d local%s, %d constant%s, %d function%s\n", S(f->locVarsSize),
         S(f->kSize), S(f->pSize));
}

static void PrintConstants(const Prototype *f) {
  int i, n = f->kSize;
  printf("constants (%d) for %p:\n", n, VOID(f));
  for (i = 0; i < n; i++) {
    printf("\t%d\t", i + 1);
    PrintConstant(f, i);
    printf("\n");
  }
}

static void PrintLocals(const Prototype *f) {
  int i, n = f->locVarsSize;
  printf("locals (%d) for %p:\n", n, VOID(f));
  for (i = 0; i < n; i++) {
    printf("\t%d\t%s\t%d\t%d\n", i, GET_STR(f->locVars[i].varname),
           f->locVars[i].startPC + 1, f->locVars[i].endPC + 1);
  }
}

static void PrintUpvalues(const Prototype *f) {
  int i, n = f->upvaluesSize;
  printf("upvalues (%d) for %p:\n", n, VOID(f));
  if (f->upvalues == NULL) {
    return;
  }
  for (i = 0; i < n; i++) {
    printf("\t%d\t%s\n", i, GET_STR(f->upvalues[i]));
  }
}

void PrintFunction(const Prototype *f, int full) {
  int i, n = f->pSize;
  PrintHeader(f);
  PrintCode(f);
  if (full) {
    PrintConstants(f);
    PrintLocals(f);
    PrintUpvalues(f);
  }
  for (i = 0; i < n; i++) {
    PrintFunction(f->inners[i], full);
  }
}
