#include "opcodes.h"

const char *const luaP_opnames[NUM_OPCODES + 1] = {
    "MOVE",     "LOADK",     "LOADBOOL", "LOADNIL",  "GETUPVAL", "GETGLOBAL",
    "GETTABLE", "SETGLOBAL", "SETUPVAL", "SETTABLE", "NEWTABLE", "SELF",
    "ADD",      "SUB",       "MUL",      "DIV",      "MOD",      "POW",
    "UNM",      "NOT",       "LEN",      "CONCAT",   "JMP",      "EQ",
    "LT",       "LE",        "TEST",     "TESTSET",  "CALL",     "TAILCALL",
    "RETURN",   "FORLOOP",   "FORPREP",  "TFORLOOP", "SETLIST",  "CLOSE",
    "CLOSURE",  "VARARG",    nullptr,
};

#define opmode(t, a, b, c, m)                                                  \
  (((t) << 7) | ((a) << 6) | ((b) << 4) | ((c) << 2) | (m))

const uint8_t luaP_opmodes[NUM_OPCODES] = {
    /*       T  A    B       C     mode		   opcode	*/
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_NOT_USED,
           FORMAT_A_B_C) /* OP_MOVE */
    ,
    opmode(0, 1, OP_ARG_CONST_OR_REG, OP_ARG_NOT_USED,
           FORMAT_A_Bx) /* OP_LOADK */
    ,
    opmode(0, 1, OP_ARG_USED, OP_ARG_USED, FORMAT_A_B_C) /* OP_LOADBOOL */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_NOT_USED,
           FORMAT_A_B_C) /* OP_LOADNIL */
    ,
    opmode(0, 1, OP_ARG_USED, OP_ARG_NOT_USED, FORMAT_A_B_C) /* OP_GETUPVAL */
    ,
    opmode(0, 1, OP_ARG_CONST_OR_REG, OP_ARG_NOT_USED,
           FORMAT_A_Bx) /* OP_GETGLOBAL */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_GETTABLE */
    ,
    opmode(0, 0, OP_ARG_CONST_OR_REG, OP_ARG_NOT_USED,
           FORMAT_A_Bx) /* OP_SETGLOBAL */
    ,
    opmode(0, 0, OP_ARG_USED, OP_ARG_NOT_USED, FORMAT_A_B_C) /* OP_SETUPVAL */
    ,
    opmode(0, 0, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_SETTABLE */
    ,
    opmode(0, 1, OP_ARG_USED, OP_ARG_USED, FORMAT_A_B_C) /* OP_NEWTABLE */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_SELF */
    ,
    opmode(0, 1, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_ADD */
    ,
    opmode(0, 1, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_SUB */
    ,
    opmode(0, 1, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_MUL */
    ,
    opmode(0, 1, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_DIV */
    ,
    opmode(0, 1, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_MOD */
    ,
    opmode(0, 1, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_POW */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_NOT_USED,
           FORMAT_A_B_C) /* OP_UNM */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_NOT_USED,
           FORMAT_A_B_C) /* OP_NOT */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_NOT_USED,
           FORMAT_A_B_C) /* OP_LEN */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_REG_OR_OFFSET,
           FORMAT_A_B_C) /* OP_CONCAT */
    ,
    opmode(0, 0, OP_ARG_REG_OR_OFFSET, OP_ARG_NOT_USED,
           FORMAT_A_sBx) /* OP_JMP */
    ,
    opmode(1, 0, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_EQ */
    ,
    opmode(1, 0, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_LT */
    ,
    opmode(1, 0, OP_ARG_CONST_OR_REG, OP_ARG_CONST_OR_REG,
           FORMAT_A_B_C) /* OP_LE */
    ,
    opmode(1, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_USED, FORMAT_A_B_C) /* OP_TEST */
    ,
    opmode(1, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_USED,
           FORMAT_A_B_C) /* OP_TESTSET */
    ,
    opmode(0, 1, OP_ARG_USED, OP_ARG_USED, FORMAT_A_B_C) /* OP_CALL */
    ,
    opmode(0, 1, OP_ARG_USED, OP_ARG_USED, FORMAT_A_B_C) /* OP_TAILCALL */
    ,
    opmode(0, 0, OP_ARG_USED, OP_ARG_NOT_USED, FORMAT_A_B_C) /* OP_RETURN */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_NOT_USED,
           FORMAT_A_sBx) /* OP_FORLOOP */
    ,
    opmode(0, 1, OP_ARG_REG_OR_OFFSET, OP_ARG_NOT_USED,
           FORMAT_A_sBx) /* OP_FORPREP */
    ,
    opmode(1, 0, OP_ARG_NOT_USED, OP_ARG_USED, FORMAT_A_B_C) /* OP_TFORLOOP */
    ,
    opmode(0, 0, OP_ARG_USED, OP_ARG_USED, FORMAT_A_B_C) /* OP_SETLIST */
    ,
    opmode(0, 0, OP_ARG_NOT_USED, OP_ARG_NOT_USED, FORMAT_A_B_C) /* OP_CLOSE */
    ,
    opmode(0, 1, OP_ARG_USED, OP_ARG_NOT_USED, FORMAT_A_Bx) /* OP_CLOSURE */
    ,
    opmode(0, 1, OP_ARG_USED, OP_ARG_NOT_USED, FORMAT_A_B_C) /* OP_VARARG */
    ,
};
