/* Some generic functions over Lua objects. */

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"

#include "intern.h"
#include "ldo.h"
#include "lstate.h"
#include "lvm.h"
#include "object.h"

const Value luaO_nilobject_ = {{nullptr}, LUA_TYPE_NIL};

/*
** converts an integer to a "floating point byte", represented as
** (eeeeexxx), where the real value is (1xxx) * 2^(eeeee - 1) if
** eeeee != 0 and (xxx) otherwise.
*/
int luaO_int2fb(unsigned int x) {
  int e = 0; /* expoent */
  while (x >= 16) {
    x = (x + 1) >> 1;
    e++;
  }
  if (x < 8) {
    return x;
  } else {
    return ((e + 1) << 3) | (cast_int(x) - 8);
  }
}

/* converts back */
int luaO_fb2int(int x) {
  int e = (x >> 3) & 31;
  if (e == 0) {
    return x;
  } else {
    return ((x & 7) + 8) << (e - 1);
  }
}

int luaO_log2(unsigned int x) {
  static const uint8_t log_2[256] = {
      0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
      5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8};
  int l = -1;
  while (x >= 256) {
    l += 8;
    x >>= 8;
  }
  return l + log_2[x];
}

int luaO_rawequalObj(const Value *t1, const Value *t2) {
  if (GET_TYPE(t1) != GET_TYPE(t2)) {
    return 0;
  } else {
    switch (GET_TYPE(t1)) {
    case LUA_TYPE_NIL:
      return 1;
    case LUA_TYPE_NUMBER:
      return luai_numeq(NUMBER_VALUE(t1), NUMBER_VALUE(t2));
    case LUA_TYPE_BOOLEAN:
      return BOOL_VALUE(t1) == BOOL_VALUE(t2); /* boolean true must be 1 !! */
    case LUA_TYPE_PTR:
      return PTR_VALUE(t1) == PTR_VALUE(t2);
    default:
      DEBUG_ASSERT(IS_COLLECTABLE(t1));
      return GC_VALUE(t1) == GC_VALUE(t2);
    }
  }
}

int luaO_str2d(const char *s, lua_Number *result) {
  char *endptr;
  *result = lua_str2number(s, &endptr);
  if (endptr == s) {
    return 0; /* conversion failed */
  }
  if (*endptr == 'x' || *endptr == 'X') { /* maybe an hexadecimal constant? */
    *result = cast_num(strtoul(s, &endptr, 16));
  }
  if (*endptr == '\0') {
    return 1; /* most common case */
  }
  while (isspace(cast(unsigned char, *endptr))) {
    endptr++;
  }
  if (*endptr != '\0') {
    return 0; /* invalid trailing characters? */
  }
  return 1;
}

static void pushstr(lua_State *L, const char *str) {
  SET_STRING_TO_STACK(L, L->top, luaS_new(L, str));
  incr_top(L);
}

/* this function handles only `%d', `%c', %f, %p, and `%s' formats */
const char *luaO_pushvfstring(lua_State *L, const char *fmt, va_list argp) {
  int n = 1;
  pushstr(L, "");
  for (;;) {
    const char *e = strchr(fmt, '%');
    if (e == NULL) {
      break;
    }
    SET_STRING_TO_STACK(L, L->top, String_intern(L, fmt, e - fmt));
    incr_top(L);
    switch (*(e + 1)) {
    case 's': {
      const char *s = va_arg(argp, char *);
      if (s == NULL) {
        s = "(null)";
      }
      pushstr(L, s);
      break;
    }
    case 'c': {
      char buff[2];
      buff[0] = cast(char, va_arg(argp, int));
      buff[1] = '\0';
      pushstr(L, buff);
      break;
    }
    case 'd': {
      SET_NUMBER(L->top, cast_num(va_arg(argp, int)));
      incr_top(L);
      break;
    }
    case 'f': {
      SET_NUMBER(L->top, cast_num(va_arg(argp, l_uacNumber)));
      incr_top(L);
      break;
    }
    case 'p': {
      // Should be enough space for a pointer.
      char buff[4 * sizeof(void *) + 8];
      snprintf(buff, sizeof(buff), "%p", va_arg(argp, void *));
      pushstr(L, buff);
      break;
    }
    case '%': {
      pushstr(L, "%");
      break;
    }
    default: {
      char buff[3];
      buff[0] = '%';
      buff[1] = *(e + 1);
      buff[2] = '\0';
      pushstr(L, buff);
      break;
    }
    }
    n += 2;
    fmt = e + 2;
  }
  pushstr(L, fmt);
  luaV_concat(L, n + 1, cast_int(L->top - L->base) - 1);
  L->top -= n;
  return GET_STR_VALUE(L->top - 1);
}

const char *luaO_pushfstring(lua_State *L, const char *fmt, ...) {
  const char *msg;
  va_list argp;
  va_start(argp, fmt);
  msg = luaO_pushvfstring(L, fmt, argp);
  va_end(argp);
  return msg;
}

void luaO_chunkid(char *out, const char *source, size_t bufflen) {
  if (*source == '=') {
    strncpy(out, source + 1, bufflen); /* remove first char */
    out[bufflen - 1] = '\0';           /* ensures null termination */
  } else {                             /* out = "source", or "...source" */
    if (*source == '@') {
      size_t l;
      source++; /* skip the `@' */
      bufflen -= sizeof(" '...' ");
      l = strlen(source);
      strcpy(out, "");
      if (l > bufflen) {
        source += (l - bufflen); /* get last part of file name */
        strcat(out, "...");
      }
      strcat(out, source);
    } else {                                /* out = [string "string"] */
      size_t len = strcspn(source, "\n\r"); /* stop at first newline */
      bufflen -= sizeof(" [string \"...\"] ");
      if (len > bufflen) {
        len = bufflen;
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
  }
}
