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

#define next(ls) (ls->current = zgetc(ls->z))

#define currIsNewline(ls) (ls->current == '\n' || ls->current == '\r')

// All tokens are ORDER RESERVED.
static const char *const TOKENS[] = {
    "and",    "break",    "do",     "else",  "elseif", "end",   "false",
    "for",    "function", "if",     "in",    "local",  "nil",   "not",
    "or",     "repeat",   "return", "then",  "true",   "until", "while",
    "..",     "...",      "==",     ">=",    "<=",     "~=",    "<number>",
    "<name>", "<string>", "<eof>",  nullptr,
};

#define saveAndNext(ls) (save(ls, ls->current), next(ls))

static void save(LexState *ls, int c) {
  StringBuilder *b = ls->buff;
  if (b->len + 1 > b->size) {
    if (b->size >= SIZE_MAX / 2) {
      Lex_throwWith(ls, "lexical element too long", 0);
    }
    size_t newSize = b->size * 2;
    StringBuilder_resize(ls->L, b, newSize);
  }
  b->str[b->len++] = cast(char, c);
}

void luaX_init(lua_State *L) {
  for (int i = 0; i < NUM_RESERVED; i++) {
    String *ts = String_create(L, TOKENS[i]);
    String_intern(ts); /* reserved words are never collected */
    assert(strlen(TOKENS[i]) + 1 <= TOKEN_LEN);
    ts->keywordID = (uint8_t)(i + 1);
  }
}

#define MAXSRC 80

const char *Lex_tokenText(LexState *ls, int token) {
  if (token < FIRST_RESERVED) {
    assert(token == (uint8_t)token);
    return iscntrl(token) ? luaO_pushfstring(ls->L, "char(%d)", token)
                          : luaO_pushfstring(ls->L, "%c", token);
  }
  return TOKENS[token - FIRST_RESERVED];
}

static const char *txtToken(LexState *ls, int token) {
  switch (token) {
  case TK_NAME:
  case TK_STRING:
  case TK_NUMBER:
    save(ls, '\0');
    return StringBuilder_get(ls->buff);
  default:
    return Lex_tokenText(ls, token);
  }
}

void Lex_throwWith(LexState *ls, const char *msg, int token) {
  char buff[MAXSRC];
  Lexer_chunkID(buff, STRING_CONTENT(ls->source), MAXSRC);
  msg = luaO_pushfstring(ls->L, "%s:%d: %s", buff, ls->linenumber, msg);
  if (token) {
    luaO_pushfstring(ls->L, "%s near '%s'", msg, txtToken(ls, token));
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

static void inclinenumber(LexState *ls) {
  int old = ls->current;
  assert(currIsNewline(ls));
  next(ls); /* skip `\n' or `\r' */
  if (currIsNewline(ls) && ls->current != old) {
    next(ls); /* skip `\n\r' or `\r\n' */
  }
  if (++ls->linenumber >= SAFE_INT_MAX) {
    Lex_throw(ls, "chunk has too many lines");
  }
}

void luaX_setinput(lua_State *L, LexState *ls, ZIO *z, String *source) {
  ls->decpoint = '.';
  ls->L = L;
  ls->lookahead.token = TK_EOS; /* no look-ahead token */
  ls->z = z;
  ls->fs = NULL;
  ls->linenumber = 1;
  ls->lastline = 1;
  ls->source = source;
  StringBuilder_resize(ls->L, ls->buff,
                       LUA_MIN_BUF_SIZE); /* initialize buffer */
  next(ls);                               /* read first char */
}

/*
** =======================================================
** LEXICAL ANALYZER
** =======================================================
*/

static int check_next(LexState *ls, const char *set) {
  if (!strchr(set, ls->current)) {
    return 0;
  }
  saveAndNext(ls);
  return 1;
}

static void buffreplace(LexState *ls, char from, char to) {
  size_t n = StringBuilder_len(ls->buff);
  char *p = StringBuilder_get(ls->buff);
  while (n--) {
    if (p[n] == from) {
      p[n] = to;
    }
  }
}

static void trydecpoint(LexState *ls, Literal *seminfo) {
  /* format error: try to update decimal point separator */
  struct lconv *cv = localeconv();
  char old = ls->decpoint;
  ls->decpoint = (cv ? cv->decimal_point[0] : '.');
  buffreplace(ls, old, ls->decpoint); /* try updated decimal separator */
  if (!luaO_str2d(StringBuilder_get(ls->buff), &seminfo->num)) {
    /* format error with correct decimal point: no more options */
    buffreplace(ls, ls->decpoint, '.'); /* undo change (for error message) */
    Lex_throwWith(ls, "malformed number", TK_NUMBER);
  }
}

/* LUA_NUMBER */
static void read_numeral(LexState *ls, Literal *seminfo) {
  assert(isdigit(ls->current));
  do {
    saveAndNext(ls);
  } while (isdigit(ls->current) || ls->current == '.');
  if (check_next(ls, "Ee")) { /* `E'? */
    check_next(ls, "+-");     /* optional exponent sign */
  }
  while (isalnum(ls->current) || ls->current == '_') {
    saveAndNext(ls);
  }
  save(ls, '\0');
  buffreplace(ls, '.', ls->decpoint); /* follow locale for decimal point */
  if (!luaO_str2d(StringBuilder_get(ls->buff),
                  &seminfo->num)) { /* format error? */
    trydecpoint(ls, seminfo);       /* try to update decimal point separator */
  }
}

static int skip_sep(LexState *ls) {
  int count = 0;
  int s = ls->current;
  assert(s == '[' || s == ']');
  saveAndNext(ls);
  while (ls->current == '=') {
    saveAndNext(ls);
    count++;
  }
  return (ls->current == s) ? count : (-count) - 1;
}

static void read_long_string(LexState *ls, Literal *seminfo, int sep) {
  int cont = 0;
  (void)(cont);            /* avoid warnings when `cont' is not used */
  saveAndNext(ls);         /* skip 2nd `[' */
  if (currIsNewline(ls)) { /* string starts with a newline? */
    inclinenumber(ls);     /* skip it */
  }
  for (;;) {
    switch (ls->current) {
    case EOZ:
      Lex_throwWith(
          ls, (seminfo) ? "unfinished long string" : "unfinished long comment",
          TK_EOS);
      break; /* to avoid warnings */
    case '[': {
      if (skip_sep(ls) == sep) {
        saveAndNext(ls); /* skip 2nd `[' */
        cont++;
        if (sep == 0) {
          Lex_throwWith(ls, "nesting of [[...]] is deprecated", '[');
        }
      }
      break;
    }
    case ']': {
      if (skip_sep(ls) == sep) {
        saveAndNext(ls); /* skip 2nd `]' */
        goto endloop;
      }
      break;
    }
    case '\n':
    case '\r': {
      save(ls, '\n');
      inclinenumber(ls);
      if (!seminfo) {
        StringBuilder_reset(ls->buff); /* avoid wasting space */
      }
      break;
    }
    default: {
      if (seminfo) {
        saveAndNext(ls);
      } else {
        next(ls);
      }
    }
    }
  }
endloop:
  if (seminfo) {
    seminfo->str = luaX_newstring(ls, StringBuilder_get(ls->buff) + (2 + sep),
                                  StringBuilder_len(ls->buff) - 2 * (2 + sep));
  }
}

static void read_string(LexState *ls, int del, Literal *seminfo) {
  saveAndNext(ls);
  while (ls->current != del) {
    switch (ls->current) {
    case EOZ:
      Lex_throwWith(ls, "unfinished string", TK_EOS);
      continue; /* to avoid warnings */
    case '\n':
    case '\r':
      Lex_throwWith(ls, "unfinished string", TK_STRING);
      continue; /* to avoid warnings */
    case '\\': {
      int c;
      next(ls); /* do not save the `\' */
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
      case '\n': /* go through */
      case '\r':
        save(ls, '\n');
        inclinenumber(ls);
        continue;
      case EOZ:
        continue; /* will raise an error next loop */
      default: {
        if (!isdigit(ls->current)) {
          saveAndNext(ls); /* handles \\, \", \', and \? */
        } else {           /* \xxx */
          int i = 0;
          c = 0;
          do {
            c = 10 * c + (ls->current - '0');
            next(ls);
          } while (++i < 3 && isdigit(ls->current));
          if (c > UCHAR_MAX) {
            Lex_throwWith(ls, "escape sequence too large", TK_STRING);
          }
          save(ls, c);
        }
        continue;
      }
      }
      save(ls, c);
      next(ls);
      continue;
    }
    default:
      saveAndNext(ls);
    }
  }
  saveAndNext(ls); /* skip delimiter */
  seminfo->str = luaX_newstring(ls, StringBuilder_get(ls->buff) + 1,
                                StringBuilder_len(ls->buff) - 2);
}

static int llex(LexState *ls, Literal *seminfo) {
  StringBuilder_reset(ls->buff);
  for (;;) {
    switch (ls->current) {
    case '\n':
    case '\r': {
      inclinenumber(ls);
      continue;
    }
    case '-': {
      next(ls);
      if (ls->current != '-') {
        return '-';
      }
      /* else is a comment */
      next(ls);
      if (ls->current == '[') {
        int sep = skip_sep(ls);
        StringBuilder_reset(ls->buff); /* `skip_sep' may dirty the buffer */
        if (sep >= 0) {
          read_long_string(ls, NULL, sep); /* long comment */
          StringBuilder_reset(ls->buff);
          continue;
        }
      }
      /* else short comment */
      while (!currIsNewline(ls) && ls->current != EOZ) {
        next(ls);
      }
      continue;
    }
    case '[': {
      int sep = skip_sep(ls);
      if (sep >= 0) {
        read_long_string(ls, seminfo, sep);
        return TK_STRING;
      } else if (sep == -1) {
        return '[';
      } else {
        Lex_throwWith(ls, "invalid long string delimiter", TK_STRING);
      }
    }
    case '=': {
      next(ls);
      if (ls->current != '=') {
        return '=';
      } else {
        next(ls);
        return TK_EQ;
      }
    }
    case '<': {
      next(ls);
      if (ls->current != '=') {
        return '<';
      } else {
        next(ls);
        return TK_LE;
      }
    }
    case '>': {
      next(ls);
      if (ls->current != '=') {
        return '>';
      } else {
        next(ls);
        return TK_GE;
      }
    }
    case '~': {
      next(ls);
      if (ls->current != '=') {
        return '~';
      } else {
        next(ls);
        return TK_NE;
      }
    }
    case '"':
    case '\'': {
      read_string(ls, ls->current, seminfo);
      return TK_STRING;
    }
    case '.': {
      saveAndNext(ls);
      if (check_next(ls, ".")) {
        if (check_next(ls, ".")) {
          return TK_DOTS; /* ... */
        } else {
          return TK_CONCAT; /* .. */
        }
      } else if (!isdigit(ls->current)) {
        return '.';
      } else {
        read_numeral(ls, seminfo);
        return TK_NUMBER;
      }
    }
    case EOZ: {
      return TK_EOS;
    }
    default: {
      if (isspace(ls->current)) {
        assert(!currIsNewline(ls));
        next(ls);
        continue;
      } else if (isdigit(ls->current)) {
        read_numeral(ls, seminfo);
        return TK_NUMBER;
      } else if (isalpha(ls->current) || ls->current == '_') {
        /* identifier or reserved word */
        String *ts;
        do {
          saveAndNext(ls);
        } while (isalnum(ls->current) || ls->current == '_');
        ts = luaX_newstring(ls, StringBuilder_get(ls->buff),
                            StringBuilder_len(ls->buff));
        if (ts->keywordID) {
          return ts->keywordID - 1 + FIRST_RESERVED;
        }
        seminfo->str = ts;
        return TK_NAME;
      } else {
        int c = ls->current;
        next(ls);
        return c; /* single-char tokens (+ - / ...) */
      }
    }
    }
  }
}

void luaX_next(LexState *ls) {
  ls->lastline = ls->linenumber;
  if (ls->lookahead.token != TK_EOS) { /* is there a look-ahead token? */
    ls->t = ls->lookahead;             /* use this one */
    ls->lookahead.token = TK_EOS;      /* and discharge it */
  } else {
    ls->t.token = llex(ls, &ls->t.literal); /* read next token */
  }
}

void luaX_lookahead(LexState *ls) {
  assert(ls->lookahead.token == TK_EOS);
  ls->lookahead.token = llex(ls, &ls->lookahead.literal);
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
