#include <errno.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "lua.h"

#include "std.h"

/*
@@ lua_tmpnam is the function that the OS library uses to create a
@* temporary name.
@@ LUA_TMPNAMBUFSIZE is the maximum size of a name created by lua_tmpnam.
** CHANGE them if you have an alternative to tmpnam (which is considered
** insecure) or if you want the original tmpnam anyway.  By default, Lua
** uses tmpnam except when POSIX is available, where it uses mkstemp.
*/
#include <unistd.h>
#define LUA_TMPNAMBUFSIZE 32
#define lua_tmpnam(b, e)                                                       \
  {                                                                            \
    strcpy(b, "/tmp/lua_XXXXXX");                                              \
    e = mkstemp(b);                                                            \
    if (e != -1)                                                               \
      close(e);                                                                \
    e = (e == -1);                                                             \
  }

static int os_pushresult(lua_State *L, int i, const char *filename) {
  int en = errno; /* calls to Lua API may change this value */
  if (i) {
    lua_pushboolean(L, 1);
    return 1;
  } else {
    lua_pushnil(L);
    lua_pushfstring(L, "%s: %s", filename, strerror(en));
    lua_pushinteger(L, en);
    return 3;
  }
}

static int os_execute(lua_State *L) {
  lua_pushinteger(L, system(luaL_optstring(L, 1, nullptr)));
  return 1;
}

static int os_remove(lua_State *L) {
  const char *filename = luaL_checkstring(L, 1);
  return os_pushresult(L, remove(filename) == 0, filename);
}

static int os_rename(lua_State *L) {
  const char *fromname = luaL_checkstring(L, 1);
  const char *toname = luaL_checkstring(L, 2);
  return os_pushresult(L, rename(fromname, toname) == 0, fromname);
}

static int os_tmpname(lua_State *L) {
  char buff[LUA_TMPNAMBUFSIZE];
  int err;
  lua_tmpnam(buff, err);
  if (err) {
    return luaL_error(L, "unable to generate a unique filename");
  }
  lua_pushstring(L, buff);
  return 1;
}

static int os_getenv(lua_State *L) {
  lua_pushstring(L, getenv(luaL_checkstring(L, 1))); /* if nullptr push nil */
  return 1;
}

static int os_clock(lua_State *L) {
  lua_pushnumber(L, ((double)clock()) / (double)CLOCKS_PER_SEC);
  return 1;
}

/*
** {======================================================
** Time/Date operations
** { year=%Y, month=%m, day=%d, hour=%H, min=%M, sec=%S,
**   wday=%w+1, yday=%j, isdst=? }
** =======================================================
*/

static void setfield(lua_State *L, const char *key, int value) {
  lua_pushinteger(L, value);
  lua_setfield(L, -2, key);
}

static void setboolfield(lua_State *L, const char *key, int value) {
  if (value < 0) { /* undefined? */
    return;        /* does not set field */
  }
  lua_pushboolean(L, value);
  lua_setfield(L, -2, key);
}

static int getboolfield(lua_State *L, const char *key) {
  int res;
  lua_getfield(L, -1, key);
  res = lua_isnil(L, -1) ? -1 : lua_toboolean(L, -1);
  lua_pop(L, 1);
  return res;
}

static int getfield(lua_State *L, const char *key, int d) {
  int res;
  lua_getfield(L, -1, key);
  if (lua_isnumber(L, -1)) {
    res = (int)lua_tointeger(L, -1);
  } else {
    if (d < 0) {
      return luaL_error(L, "field '%s' missing in date table", key);
    }
    res = d;
  }
  lua_pop(L, 1);
  return res;
}

static int os_date(lua_State *L) {
  const char *s = luaL_optstring(L, 1, "%c");
  time_t t = luaL_opt(L, (time_t)luaL_checknumber, 2, time(nullptr));
  struct tm *stm;
  if (*s == '!') { /* UTC? */
    stm = gmtime(&t);
    s++; /* skip `!' */
  } else {
    stm = localtime(&t);
  }
  if (stm == nullptr) { /* invalid date? */
    lua_pushnil(L);
  } else if (strcmp(s, "*t") == 0) {
    lua_createtable(L, 0, 9); /* 9 = number of fields */
    setfield(L, "sec", stm->tm_sec);
    setfield(L, "min", stm->tm_min);
    setfield(L, "hour", stm->tm_hour);
    setfield(L, "day", stm->tm_mday);
    setfield(L, "month", stm->tm_mon + 1);
    setfield(L, "year", stm->tm_year + 1900);
    setfield(L, "wday", stm->tm_wday + 1);
    setfield(L, "yday", stm->tm_yday + 1);
    setboolfield(L, "isdst", stm->tm_isdst);
  } else {
    char cc[3];
    luaL_Buffer b;
    cc[0] = '%';
    cc[2] = '\0';
    luaL_buffinit(L, &b);
    for (; *s; s++) {
      if (*s != '%' || *(s + 1) == '\0') { /* no conversion specifier? */
        luaL_addchar(&b, *s);
      } else {
        size_t reslen;
        char buff[200]; /* should be big enough for any conversion result */
        cc[1] = *(++s);
        reslen = strftime(buff, sizeof(buff), cc, stm);
        luaL_addlstring(&b, buff, reslen);
      }
    }
    luaL_pushresult(&b);
  }
  return 1;
}

static int os_time(lua_State *L) {
  time_t t;
  if (lua_isnoneornil(L, 1)) { /* called without args? */
    t = time(nullptr);         /* get current time */
  } else {
    struct tm ts;
    luaL_checktype(L, 1, LUA_TYPE_TABLE);
    lua_settop(L, 1); /* make sure table is at the top */
    ts.tm_sec = getfield(L, "sec", 0);
    ts.tm_min = getfield(L, "min", 0);
    ts.tm_hour = getfield(L, "hour", 12);
    ts.tm_mday = getfield(L, "day", -1);
    ts.tm_mon = getfield(L, "month", -1) - 1;
    ts.tm_year = getfield(L, "year", -1) - 1900;
    ts.tm_isdst = getboolfield(L, "isdst");
    t = mktime(&ts);
  }
  if (t == (time_t)(-1)) {
    lua_pushnil(L);
  } else {
    lua_pushnumber(L, (double)t);
  }
  return 1;
}

static int os_difftime(lua_State *L) {
  lua_pushnumber(L, difftime((time_t)(luaL_checknumber(L, 1)),
                             (time_t)(luaL_optnumber(L, 2, 0))));
  return 1;
}

/* }====================================================== */

static int os_setlocale(lua_State *L) {
  static const int cat[] = {
      LC_ALL, LC_COLLATE, LC_CTYPE, LC_MONETARY, LC_NUMERIC, LC_TIME,
  };
  static const char *const catnames[] = {
      "all", "collate", "ctype", "monetary", "numeric", "time", nullptr,
  };
  const char *l = luaL_optstring(L, 1, nullptr);
  int op = luaL_checkoption(L, 2, "all", catnames);
  lua_pushstring(L, setlocale(cat[op], l));
  return 1;
}

static int os_exit(lua_State *L) { exit(luaL_optint(L, 1, EXIT_SUCCESS)); }

static const luaL_Reg syslib[] = {
    {"clock", os_clock},         {"date", os_date},
    {"difftime", os_difftime},   {"execute", os_execute},
    {"exit", os_exit},           {"getenv", os_getenv},
    {"remove", os_remove},       {"rename", os_rename},
    {"setlocale", os_setlocale}, {"time", os_time},
    {"tmpname", os_tmpname},     {nullptr, nullptr},
};

/* }====================================================== */

LUALIB_API int luaopen_os(lua_State *L) {
  luaL_register(L, "os", syslib);
  return 1;
}
