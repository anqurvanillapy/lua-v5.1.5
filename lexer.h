// Lexer.

#pragma once

#include "buffer.h"
#include "object.h"

#define FIRST_RESERVED 257

/* maximum length of a reserved word */
#define TOKEN_LEN (sizeof("function") / sizeof(char))

/*
 * WARNING: if you change the order of this enumeration,
 * grep "ORDER RESERVED"
 */
enum RESERVED {
  /* terminal symbols denoted by reserved words */
  TK_AND = FIRST_RESERVED,
  TK_BREAK,
  TK_DO,
  TK_ELSE,
  TK_ELSEIF,
  TK_END,
  TK_FALSE,
  TK_FOR,
  TK_FUNCTION,
  TK_IF,
  TK_IN,
  TK_LOCAL,
  TK_NIL,
  TK_NOT,
  TK_OR,
  TK_REPEAT,
  TK_RETURN,
  TK_THEN,
  TK_TRUE,
  TK_UNTIL,
  TK_WHILE,
  /* other terminal symbols */
  TK_CONCAT,
  TK_DOTS,
  TK_EQ,
  TK_GE,
  TK_LE,
  TK_NE,
  TK_NUMBER,
  TK_NAME,
  TK_STRING,
  TK_EOS
};

// Number of reserved keywords.
#define NUM_RESERVED (cast(int, TK_WHILE - FIRST_RESERVED + 1))

typedef union Literal {
  double num;
  String *str;
} Literal;

typedef struct Token {
  int token;
  Literal literal;
} Token;

typedef struct LexState {
  int current;          /* current character (charint) */
  int linenumber;       /* input line counter */
  int lastline;         /* line of last token `consumed' */
  Token t;              /* current token */
  Token lookahead;      /* look ahead token */
  struct FuncState *fs; /* `FuncState' is private to the parser */
  struct lua_State *L;
  ZIO *z;              /* input stream */
  StringBuilder *buff; /* buffer for tokens */
  String *source;      /* current source name */
  char decpoint;       /* locale decimal point */
} LexState;

LUAI_FUNC void luaX_init(lua_State *L);
LUAI_FUNC void luaX_setinput(lua_State *L, LexState *ls, ZIO *z,
                             String *source);
LUAI_FUNC String *luaX_newstring(LexState *ls, const char *str, size_t l);
LUAI_FUNC void luaX_next(LexState *ls);
LUAI_FUNC void luaX_lookahead(LexState *ls);
LUAI_FUNC void Lex_throwWith(LexState *ls, const char *msg, int token);
LUAI_FUNC void Lex_throw(LexState *ls, const char *errmsg);
LUAI_FUNC const char *Lex_tokenText(LexState *ls, int token);
LUAI_FUNC void Lexer_chunkID(char *out, const char *source, size_t bufSize);
