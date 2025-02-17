/* Some generic functions over Lua objects. */

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"

#include "intern.h"
#include "object.h"
#include "stack.h"
#include "state.h"
#include "vm.h"

const Value valueNil = {{nullptr}, LUA_TYPE_NIL};

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
    return ((e + 1) << 3) | ((int)x - 8);
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

bool Object_rawEqual(const Value *t1, const Value *t2) {
  if (GET_TYPE(t1) != GET_TYPE(t2)) {
    return false;
  }
  switch (GET_TYPE(t1)) {
  case LUA_TYPE_NIL:
    return true;
  case LUA_TYPE_NUMBER:
    return NUMBER_VALUE(t1) == NUMBER_VALUE(t2);
  case LUA_TYPE_BOOLEAN:
    return BOOL_VALUE(t1) == BOOL_VALUE(t2);
  case LUA_TYPE_PTR:
    return PTR_VALUE(t1) == PTR_VALUE(t2);
  default:
    assert(IS_COLLECTABLE(t1));
    return GC_VALUE(t1) == GC_VALUE(t2);
  }
}

int luaO_str2d(const char *s, double *result) {
  char *endptr;
  *result = lua_str2number(s, &endptr);
  if (endptr == s) {
    return 0; /* conversion failed */
  }
  if (*endptr == 'x' || *endptr == 'X') { /* maybe an hexadecimal constant? */
    *result = (double)strtoul(s, &endptr, 16);
  }
  if (*endptr == '\0') {
    return 1; /* most common case */
  }
  while (isspace(*endptr)) {
    endptr++;
  }
  if (*endptr != '\0') {
    return 0; /* invalid trailing characters? */
  }
  return 1;
}

static void pushStr(lua_State *L, const char *str) {
  SET_STRING_TO_STACK(L, L->top, String_create(L, str));
  incr_top(L);
}

const char *Object_vsprintf(lua_State *L, const char *fmt, va_list argp) {
  int n = 1;
  pushStr(L, "");
  while (true) {
    const char *e = strchr(fmt, '%');
    if (e == nullptr) {
      break;
    }
    SET_STRING_TO_STACK(L, L->top, String_createSized(L, fmt, e - fmt));
    incr_top(L);
    switch (*(e + 1)) {
    case 's': {
      const char *s = va_arg(argp, char *);
      if (s == nullptr) {
        s = "(null)";
      }
      pushStr(L, s);
      break;
    }
    case 'c':
      const char c[] = {(char)va_arg(argp, int), '\0'};
      pushStr(L, c);
      break;
    case 'd':
      SET_NUMBER(L->top, va_arg(argp, int));
      incr_top(L);
      break;
    case 'f':
      SET_NUMBER(L->top, va_arg(argp, double));
      incr_top(L);
      break;
    case 'p':
      // Should be enough space for a pointer.
      char ptr[4 * sizeof(void *) + 8];
      snprintf(ptr, sizeof(ptr), "%p", va_arg(argp, void *));
      pushStr(L, ptr);
      break;
    case '%':
      pushStr(L, "%");
      break;
    default:
      const char raw[] = {'%', *(e + 1), '\0'};
      pushStr(L, raw);
      break;
    }
    n += 2;
    fmt = e + 2;
  }
  pushStr(L, fmt);
  luaV_concat(L, n + 1, (int)(L->top - L->base) - 1);
  L->top -= n;
  return VALUE_STRING_CONTENT(L->top - 1);
}

const char *Object_sprintf(lua_State *L, const char *fmt, ...) {
  va_list argp;
  va_start(argp, fmt);
  const char *msg = Object_vsprintf(L, fmt, argp);
  va_end(argp);
  return msg;
}
