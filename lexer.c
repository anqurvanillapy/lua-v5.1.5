#include <ctype.h>
#include <locale.h>

#include "buffer.h"
#include "intern.h"
#include "lexer.h"
#include "object.h"
#include "parser.h"
#include "stack.h"
#include "state.h"
#include "table.h"

#define NEXT(ls) (ls->current = READ_CHAR(ls->z))
#define IS_CUR_NEWLINE(ls) (ls->current == '\n' || ls->current == '\r')

// All tokens are ORDER RESERVED.
static const char *const TOKENS[] = {
    "and",    "break",    "do",     "else",  "elseif", "end",   "false",
    "for",    "function", "if",     "in",    "local",  "nil",   "not",
    "or",     "repeat",   "return", "then",  "true",   "until", "while",
    "..",     "...",      "==",     ">=",    "<=",     "~=",    "<number>",
    "<name>", "<string>", "<eof>",  nullptr,
};

#define SAVE_AND_NEXT(ls)                                                      \
  do {                                                                         \
    save(ls, ls->current);                                                     \
    NEXT(ls);                                                                  \
  } while (false)

static void save(LexState *ls, int c) {
  StringBuilder *b = ls->tokens;
  if (b->len + 1 > b->size) {
    if (b->size >= SIZE_MAX / 2) {
      Lex_throwWith(ls, "lexical element too long", 0);
    }
    size_t newSize = b->size * 2;
    StringBuilder_resize(ls->L, b, newSize);
  }
  b->str[b->len++] = (char)c;
}

void Lexer_init(lua_State *L) {
  for (size_t i = 0; i < NUM_RESERVED; i++) {
    String *ts = String_create(L, TOKENS[i]);
    String_intern(ts);
    assert(strlen(TOKENS[i]) + 1 <= TOKEN_LEN);
    ts->keywordID = (uint8_t)(i + 1);
  }
}

const char *Lex_tokenText(LexState *ls, int token) {
  if (token < FIRST_RESERVED) {
    assert(token == (uint8_t)token);
    return iscntrl(token) ? Object_sprintf(ls->L, "char(%d)", token)
                          : Object_sprintf(ls->L, "%c", token);
  }
  return TOKENS[token - FIRST_RESERVED];
}

static const char *txtToken(LexState *ls, int token) {
  switch (token) {
  case TK_NAME:
  case TK_STRING:
  case TK_NUMBER:
    save(ls, '\0');
    return StringBuilder_get(ls->tokens);
  default:
    return Lex_tokenText(ls, token);
  }
}

void Lex_throwWith(LexState *ls, const char *msg, int token) {
  char buff[80];
  Lexer_chunkID(buff, STRING_CONTENT(ls->source), sizeof(buff));
  msg = Object_sprintf(ls->L, "%s:%d: %s", buff, ls->linenumber, msg);
  if (token) {
    Object_sprintf(ls->L, "%s near '%s'", msg, txtToken(ls, token));
  }
  luaD_throw(ls->L, LUA_ERRSYNTAX);
}

void Lex_throw(LexState *ls, const char *errmsg) {
  Lex_throwWith(ls, errmsg, ls->t.token);
}

String *luaX_newstring(LexState *ls, const char *str, size_t l) {
  lua_State *L = ls->L;
  String *ts = String_createSized(L, str, l);
  Value *o = Table_insertString(L, ls->fs->h, ts); /* entry for `str' */
  if (IS_TYPE_NIL(o)) {
    SET_BOOL(o, 1); /* make sure `str' will not be collected */
    luaC_checkGC(L);
  }
  return ts;
}

static void incLineNumber(LexState *ls) {
  int old = ls->current;
  assert(IS_CUR_NEWLINE(ls));
  // Skip `\n` or `\r`.
  NEXT(ls);
  if (IS_CUR_NEWLINE(ls) && ls->current != old) {
    // skip `\n\r` or `\r\n`.
    NEXT(ls);
  }
  if (++ls->linenumber >= SAFE_INT_MAX) {
    Lex_throw(ls, "chunk has too many lines");
  }
}

void Lexer_setInput(lua_State *L, LexState *ls, ZIO *z, String *source) {
  ls->dec_point = '.';
  ls->L = L;
  // No look-ahead token.
  ls->lookahead.token = TK_EOS;
  ls->z = z;
  ls->fs = nullptr;
  ls->linenumber = 1;
  ls->lastline = 1;
  ls->source = source;
  StringBuilder_resize(ls->L, ls->tokens, LUA_MIN_BUF_SIZE);
  // Read first char.
  NEXT(ls);
}

static bool checkNextContains(LexState *ls, const char *set) {
  if (!strchr(set, ls->current)) {
    return false;
  }
  SAVE_AND_NEXT(ls);
  return true;
}

static void replaceAll(LexState *ls, char from, char to) {
  if (from == to) {
    return;
  }
  size_t n = StringBuilder_len(ls->tokens);
  char *p = StringBuilder_get(ls->tokens);
  while (n--) {
    if (p[n] == from) {
      p[n] = to;
    }
  }
}

static void tryLocaleDecPoint(LexState *ls, Literal *lit) {
  // Format error: try to update the decimal point separator.
  struct lconv *cv = localeconv();
  char old = ls->dec_point;
  ls->dec_point = cv ? cv->decimal_point[0] : '.';
  replaceAll(ls, old, ls->dec_point);

  if (!luaO_str2d(StringBuilder_get(ls->tokens), &lit->num)) {
    // Still getting format error with the correct decimal point.

    // Undo changes (for error messages).
    replaceAll(ls, ls->dec_point, '.');
    Lex_throwWith(ls, "malformed number", TK_NUMBER);
  }
}

static void readNumber(LexState *ls, Literal *lit) {
  assert(isdigit(ls->current));
  do {
    SAVE_AND_NEXT(ls);
  } while (isdigit(ls->current) || ls->current == '.');
  if (checkNextContains(ls, "Ee")) {
    checkNextContains(ls, "+-");
  }
  while (isalnum(ls->current) || ls->current == '_') {
    SAVE_AND_NEXT(ls);
  }
  save(ls, '\0');

  replaceAll(ls, '.', ls->dec_point);
  if (!luaO_str2d(StringBuilder_get(ls->tokens), &lit->num)) {
    tryLocaleDecPoint(ls, lit);
  }
}

static int skipSep(LexState *ls) {
  int count = 0;
  int s = ls->current;
  assert(s == '[' || s == ']');
  SAVE_AND_NEXT(ls);
  while (ls->current == '=') {
    SAVE_AND_NEXT(ls);
    count++;
  }
  return ls->current == s ? count : -count - 1;
}

static void readLongString(LexState *ls, Literal *lit, int sep) {
  // Skip 2nd `[`.
  SAVE_AND_NEXT(ls);
  if (IS_CUR_NEWLINE(ls)) {
    incLineNumber(ls);
  }
  while (true) {
    switch (ls->current) {
    case EOZ:
      Lex_throwWith(ls,
                    lit ? "unfinished long string" : "unfinished long comment",
                    TK_EOS);
      continue;
    case '[':
      if (skipSep(ls) == sep) {
        // Skip 2nd `[`.
        SAVE_AND_NEXT(ls);
        if (sep == 0) {
          Lex_throwWith(ls, "nesting of [[...]] is deprecated", '[');
        }
      }
      break;
    case ']':
      if (skipSep(ls) == sep) {
        // Skip 2nd `]`.
        SAVE_AND_NEXT(ls);
        goto endloop;
      }
      break;
    case '\n':
    case '\r':
      save(ls, '\n');
      incLineNumber(ls);
      if (!lit) {
        // Avoid wasting space.
        StringBuilder_reset(ls->tokens);
      }
      break;
    default:
      if (lit) {
        SAVE_AND_NEXT(ls);
      } else {
        NEXT(ls);
      }
    }
  }

endloop:
  if (lit) {
    lit->str = luaX_newstring(ls, StringBuilder_get(ls->tokens) + (2 + sep),
                              StringBuilder_len(ls->tokens) - 2 * (2 + sep));
  }
}

static void readString(LexState *ls, int del, Literal *lit) {
  SAVE_AND_NEXT(ls);
  while (ls->current != del) {
    switch (ls->current) {
    case EOZ:
      Lex_throwWith(ls, "unfinished string", TK_EOS);
      continue;
    case '\n':
    case '\r':
      Lex_throwWith(ls, "unfinished string", TK_STRING);
      continue;
    case '\\':
      int c;
      // Do not save the `\`.
      NEXT(ls);
      switch (ls->current) {
      case 'a':
        c = '\a';
        break;
      case 'b':
        c = '\b';
        break;
      case 'f':
        c = '\f';
        break;
      case 'n':
        c = '\n';
        break;
      case 'r':
        c = '\r';
        break;
      case 't':
        c = '\t';
        break;
      case 'v':
        c = '\v';
        break;
      case '\n':
      case '\r':
        save(ls, '\n');
        incLineNumber(ls);
        continue;
      case EOZ:
        // Will raise an error next loop.
        continue;
      default:
        if (!isdigit(ls->current)) {
          // Handles \\, \", \', and \?
          SAVE_AND_NEXT(ls);
        } else {
          // \xxx
          int i = 0;
          c = 0;
          do {
            c = 10 * c + (ls->current - '0');
            NEXT(ls);
          } while (++i < 3 && isdigit(ls->current));
          if (c > UCHAR_MAX) {
            Lex_throwWith(ls, "escape sequence too large", TK_STRING);
          }
          save(ls, c);
        }
        continue;
      }
      save(ls, c);
      NEXT(ls);
      continue;
    default:
      SAVE_AND_NEXT(ls);
    }
  }
  // Skip delimiter.
  SAVE_AND_NEXT(ls);
  lit->str = luaX_newstring(ls, StringBuilder_get(ls->tokens) + 1,
                            StringBuilder_len(ls->tokens) - 2);
}

static int doLex(LexState *ls, Literal *lit) {
  StringBuilder_reset(ls->tokens);
  while (true) {
    switch (ls->current) {
    case '\n':
    case '\r':
      incLineNumber(ls);
      continue;
    case '-':
      NEXT(ls);
      if (ls->current != '-') {
        return '-';
      }
      // Comment.
      NEXT(ls);
      if (ls->current == '[') {
        int sep = skipSep(ls);
        // skipSep may dirty the buffer.
        StringBuilder_reset(ls->tokens);
        if (sep >= 0) {
          readLongString(ls, nullptr, sep);
          StringBuilder_reset(ls->tokens);
          continue;
        }
      }
      // Short comment.
      while (!IS_CUR_NEWLINE(ls) && ls->current != EOZ) {
        NEXT(ls);
      }
      continue;
    case '[':
      int sep = skipSep(ls);
      if (sep >= 0) {
        readLongString(ls, lit, sep);
        return TK_STRING;
      }
      if (sep == -1) {
        return '[';
      }
      Lex_throwWith(ls, "invalid long string delimiter", TK_STRING);
      continue;
    case '=':
      NEXT(ls);
      if (ls->current != '=') {
        return '=';
      }
      NEXT(ls);
      return TK_EQ;
    case '<':
      NEXT(ls);
      if (ls->current != '=') {
        return '<';
      }
      NEXT(ls);
      return TK_LE;
    case '>':
      NEXT(ls);
      if (ls->current != '=') {
        return '>';
      }
      NEXT(ls);
      return TK_GE;
    case '~':
      NEXT(ls);
      if (ls->current != '=') {
        return '~';
      }
      NEXT(ls);
      return TK_NE;
    case '"':
    case '\'':
      readString(ls, ls->current, lit);
      return TK_STRING;
    case '.':
      SAVE_AND_NEXT(ls);
      if (checkNextContains(ls, ".")) {
        if (checkNextContains(ls, ".")) {
          return TK_DOTS; /* ... */
        }
        return TK_CONCAT; /* .. */
      }
      if (!isdigit(ls->current)) {
        return '.';
      }
      readNumber(ls, lit);
      return TK_NUMBER;
    case EOZ:
      return TK_EOS;
    default:
      if (isspace(ls->current)) {
        assert(!IS_CUR_NEWLINE(ls));
        NEXT(ls);
        continue;
      }
      if (isdigit(ls->current)) {
        readNumber(ls, lit);
        return TK_NUMBER;
      }
      if (isalpha(ls->current) || ls->current == '_') {
        // Identifier or reserved word.
        String *ts;
        do {
          SAVE_AND_NEXT(ls);
        } while (isalnum(ls->current) || ls->current == '_');
        ts = luaX_newstring(ls, StringBuilder_get(ls->tokens),
                            StringBuilder_len(ls->tokens));
        if (ts->keywordID) {
          return ts->keywordID - 1 + FIRST_RESERVED;
        }
        lit->str = ts;
        return TK_NAME;
      }
      int c = ls->current;
      NEXT(ls);
      // Single-char tokens, e.g. `+`.
      return c;
    }
  }
}

void Lexer_next(LexState *ls) {
  ls->lastline = ls->linenumber;
  if (ls->lookahead.token != TK_EOS) {
    ls->t = ls->lookahead;
    ls->lookahead.token = TK_EOS;
  } else {
    ls->t.token = doLex(ls, &ls->t.literal);
  }
}

void Lexer_lookahead(LexState *ls) {
  assert(ls->lookahead.token == TK_EOS);
  ls->lookahead.token = doLex(ls, &ls->lookahead.literal);
}

void Lexer_chunkID(char *out, const char *source, size_t bufSize) {
  if (*source == '=') {
    // Remove first char.
    strncpy(out, source + 1, bufSize);
    // Ensures null termination.
    out[bufSize - 1] = '\0';
    return;
  }
  // out = "source", or "...source".
  if (*source == '@') {
    source++; /* skip the `@' */
    bufSize -= sizeof(" '...' ");
    size_t l = strlen(source);
    strcpy(out, "");
    if (l > bufSize) {
      source += (l - bufSize); /* get last part of file name */
      strcat(out, "...");
    }
    strcat(out, source);
    return;
  }
  // out = [string "string"].
  size_t len = strcspn(source, "\n\r"); /* stop at first newline */
  bufSize -= sizeof(" [string \"...\"] ");
  if (len > bufSize) {
    len = bufSize;
  }
  strcpy(out, "[string \"");
  if (source[len] != '\0') { /* must truncate? */
    strncat(out, source, len);
    strcat(out, "...");
  } else {
    strcat(out, source);
  }
  strcat(out, "\"]");
}
