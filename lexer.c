#include <ctype.h>
#include <locale.h>
#include <string.h>

#include "lua.h"

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
  Mbuffer *b = ls->buff;
  if (b->n + 1 > b->buffsize) {
    if (b->buffsize >= SIZE_MAX / 2) {
      Lex_throwWith(ls, "lexical element too long", 0);
    }
    size_t newSize = b->buffsize * 2;
    luaZ_resizebuffer(ls->L, b, newSize);
  }
  b->buffer[b->n++] = cast(char, c);
}

void luaX_init(lua_State *L) {
  for (int i = 0; i < NUM_RESERVED; i++) {
    String *ts = String_create(L, TOKENS[i]);
    String_intern(ts); /* reserved words are never collected */
    DEBUG_ASSERT(strlen(TOKENS[i]) + 1 <= TOKEN_LEN);
    ts->keywordID = (uint8_t)(i + 1);
  }
}

#define MAXSRC 80

const char *luaX_token2str(LexState *ls, int token) {
  if (token < FIRST_RESERVED) {
    DEBUG_ASSERT(token == cast(unsigned char, token));
    return (iscntrl(token)) ? luaO_pushfstring(ls->L, "char(%d)", token)
                            : luaO_pushfstring(ls->L, "%c", token);
  } else {
    return TOKENS[token - FIRST_RESERVED];
  }
}

static const char *txtToken(LexState *ls, int token) {
  switch (token) {
  case TK_NAME:
  case TK_STRING:
  case TK_NUMBER:
    save(ls, '\0');
    return luaZ_buffer(ls->buff);
  default:
    return luaX_token2str(ls, token);
  }
}

void Lex_throwWith(LexState *ls, const char *msg, int token) {
  char buff[MAXSRC];
  luaO_chunkid(buff, STRING_CONTENT(ls->source), MAXSRC);
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
  Value *o = luaH_setstr(L, ls->fs->h, ts); /* entry for `str' */
  if (IS_TYPE_NIL(o)) {
    SET_BOOL(o, 1); /* make sure `str' will not be collected */
    luaC_checkGC(L);
  }
  return ts;
}

static void inclinenumber(LexState *ls) {
  int old = ls->current;
  DEBUG_ASSERT(currIsNewline(ls));
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
  luaZ_resizebuffer(ls->L, ls->buff, LUA_MINBUFFER); /* initialize buffer */
  next(ls);                                          /* read first char */
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
  size_t n = luaZ_bufflen(ls->buff);
  char *p = luaZ_buffer(ls->buff);
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
  if (!luaO_str2d(luaZ_buffer(ls->buff), &seminfo->num)) {
    /* format error with correct decimal point: no more options */
    buffreplace(ls, ls->decpoint, '.'); /* undo change (for error message) */
    Lex_throwWith(ls, "malformed number", TK_NUMBER);
  }
}

/* LUA_NUMBER */
static void read_numeral(LexState *ls, Literal *seminfo) {
  DEBUG_ASSERT(isdigit(ls->current));
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
  if (!luaO_str2d(luaZ_buffer(ls->buff), &seminfo->num)) { /* format error? */
    trydecpoint(ls, seminfo); /* try to update decimal point separator */
  }
}

static int skip_sep(LexState *ls) {
  int count = 0;
  int s = ls->current;
  DEBUG_ASSERT(s == '[' || s == ']');
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
        luaZ_resetbuffer(ls->buff); /* avoid wasting space */
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
    seminfo->str = luaX_newstring(ls, luaZ_buffer(ls->buff) + (2 + sep),
                                  luaZ_bufflen(ls->buff) - 2 * (2 + sep));
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
  seminfo->str =
      luaX_newstring(ls, luaZ_buffer(ls->buff) + 1, luaZ_bufflen(ls->buff) - 2);
}

static int llex(LexState *ls, Literal *seminfo) {
  luaZ_resetbuffer(ls->buff);
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
        luaZ_resetbuffer(ls->buff); /* `skip_sep' may dirty the buffer */
        if (sep >= 0) {
          read_long_string(ls, NULL, sep); /* long comment */
          luaZ_resetbuffer(ls->buff);
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
        DEBUG_ASSERT(!currIsNewline(ls));
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
        ts = luaX_newstring(ls, luaZ_buffer(ls->buff), luaZ_bufflen(ls->buff));
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
  DEBUG_ASSERT(ls->lookahead.token == TK_EOS);
  ls->lookahead.token = llex(ls, &ls->lookahead.literal);
}
