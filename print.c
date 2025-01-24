#include <ctype.h>
#include <stdio.h>

#include "debug.h"
#include "object.h"
#include "opcodes.h"

static void printQuotedString(const String *ts) {
  const char *s = STRING_CONTENT(ts);
  putchar('"');
  for (size_t i = 0; i < ts->len; i++) {
    int c = (uint8_t)s[i];
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

static void printLiteral(const Prototype *f, size_t i) {
  const Value *o = &f->constants[i];
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
    printQuotedString(STRING_VALUE(o));
    break;
  default:
    printf("? type=%d", GET_TYPE(o));
    break;
  }
}

static void printLiterals(const Prototype *f) {
  printf("constants (%zu) for %p:\n", f->constantsSize, (const void *)f);
  for (size_t i = 0; i < f->constantsSize; i++) {
    printf("\t%zu\t", i + 1);
    printLiteral(f, i);
    printf("\n");
  }
}

static void printCode(const Prototype *f) {
  const Instruction *code = f->code;
  for (size_t pc = 0; pc < f->codeSize; pc++) {
    Instruction i = code[pc];
    OpCode o = GET_OPCODE(i);
    int a = GETARG_A(i);
    int b = GETARG_B(i);
    int c = GETARG_C(i);
    int bx = GETARG_Bx(i);
    int sbx = GETARG_sBx(i);
    int line = getline(f, pc);
    printf("\t%zu\t", pc + 1);
    if (line > 0) {
      printf("[%d]\t", line);
    } else {
      printf("[-]\t");
    }
    printf("%-9s\t", luaP_opnames[o]);
    switch (GET_OP_MODE(o)) {
    case FORMAT_A_B_C:
      printf("%d", a);
      if (GET_B_MODE(o) != OP_ARG_NOT_USED) {
        printf(" %zu", ISK(b) ? (-1 - INDEXK(b)) : b);
      }
      if (GET_C_MODE(o) != OP_ARG_NOT_USED) {
        printf(" %zu", ISK(c) ? (-1 - INDEXK(c)) : c);
      }
      break;
    case FORMAT_A_Bx:
      if (GET_B_MODE(o) == OP_ARG_CONST_OR_REG) {
        printf("%d %d", a, -1 - bx);
      } else {
        printf("%d %d", a, bx);
      }
      break;
    case FORMAT_A_sBx:
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
      printLiteral(f, bx);
      break;
    case OP_GETUPVAL:
    case OP_SETUPVAL:
      printf("\t; %s",
             (f->upvaluesSize > 0) ? STRING_CONTENT(f->upvalues[b]) : "-");
      break;
    case OP_GETGLOBAL:
    case OP_SETGLOBAL:
      printf("\t; %s", VALUE_STRING_CONTENT(&f->constants[bx]));
      break;
    case OP_GETTABLE:
    case OP_SELF:
      if (ISK(c)) {
        printf("\t; ");
        printLiteral(f, INDEXK(c));
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
          printLiteral(f, INDEXK(b));
        } else {
          printf("-");
        }
        printf(" ");
        if (ISK(c)) {
          printLiteral(f, INDEXK(c));
        } else {
          printf("-");
        }
      }
      break;
    case OP_JMP:
    case OP_FORLOOP:
    case OP_FORPREP:
      printf("\t; to %zu", sbx + pc + 2);
      break;
    case OP_CLOSURE:
      printf("\t; %p", (const void *)f->inners[bx]);
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

static void printHeader(const Prototype *f) {
  const char *s = STRING_CONTENT(f->source);
  if (*s == '@' || *s == '=') {
    s++;
  } else if (*s == LUA_SIGNATURE[0]) {
    s = "(bstring)";
  } else {
    s = "(string)";
  }
  printf("\n%s <%s:%d,%d> (%zu instruction%s, %zu bytes at %p)\n",
         (f->lineDefined == 0) ? "main" : "function", s, f->lineDefined,
         f->lineDefinedLast, S(f->codeSize), f->codeSize * sizeof(Instruction),
         (const void *)f);
  printf("%d%s param%s, %d slot%s, %zu upvalue%s, ", f->paramsNum,
         f->varargMode ? "+" : "", SS(f->paramsNum), S(f->maxStackSize),
         S(f->upvaluesNum));
  printf("%zu local%s, %zu constant%s, %zu function%s\n", S(f->locVarsSize),
         S(f->constantsSize), S(f->innersSize));
}

static void PrintLocals(const Prototype *f) {
  printf("locals (%zu) for %p:\n", f->locVarsSize, (const void *)f);
  for (size_t i = 0; i < f->locVarsSize; i++) {
    printf("\t%zu\t%s\t%d\t%d\n", i, STRING_CONTENT(f->locVars[i].name),
           f->locVars[i].startPC + 1, f->locVars[i].endPC + 1);
  }
}

static void PrintUpvalues(const Prototype *f) {
  printf("upvalues (%zu) for %p:\n", f->upvaluesSize, (const void *)f);
  if (f->upvalues == nullptr) {
    return;
  }
  for (size_t i = 0; i < f->upvaluesSize; i++) {
    printf("\t%zu\t%s\n", i, STRING_CONTENT(f->upvalues[i]));
  }
}

void luaU_print(const Prototype *f, int full) {
  printHeader(f);
  printCode(f);
  if (full) {
    printLiterals(f);
    PrintLocals(f);
    PrintUpvalues(f);
  }
  for (size_t i = 0; i < f->innersSize; i++) {
    luaU_print(f->inners[i], full);
  }
}
