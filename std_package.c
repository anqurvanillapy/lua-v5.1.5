#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>

#include "std.h"

/* prefix for open functions in C LIBS */
#define LUA_POF "luaopen_"

/* separator for open functions in C LIBS */
#define LUA_OFSEP "_"

#define LIBPREFIX "LOADLIB: "

#define POF LUA_POF
#define LIB_FAIL "open"

/* error codes for ll_loadfunc */
#define ERRLIB 1
#define ERRFUNC 2

#define setprogdir(L) ((void)0)

static void ll_unloadlib(void *lib);
static void *ll_load(lua_State *L, const char *path);
static lua_CFunction ll_sym(lua_State *L, void *lib, const char *sym);

static void ll_unloadlib(void *lib) { dlclose(lib); }

static void *ll_load(lua_State *L, const char *path) {
  void *lib = dlopen(path, RTLD_NOW);
  if (lib == nullptr)
    lua_pushstring(L, dlerror());
  return lib;
}

static lua_CFunction ll_sym(lua_State *L, void *lib, const char *sym) {
  lua_CFunction f = (lua_CFunction)dlsym(lib, sym);
  if (f == nullptr)
    lua_pushstring(L, dlerror());
  return f;
}

static void **ll_register(lua_State *L, const char *path) {
  void **plib;
  lua_pushfstring(L, "%s%s", LIBPREFIX, path);
  lua_gettable(L, LUA_REGISTRYINDEX); /* check library in registry? */
  if (!lua_isnil(L, -1)) {            /* is there an entry? */
    plib = (void **)lua_touserdata(L, -1);
  } else { /* no entry yet; create one */
    lua_pop(L, 1);
    plib = (void **)lua_newuserdata(L, sizeof(const void *));
    *plib = NULL;
    luaL_getmetatable(L, "_LOADLIB");
    lua_setmetatable(L, -2);
    lua_pushfstring(L, "%s%s", LIBPREFIX, path);
    lua_pushvalue(L, -2);
    lua_settable(L, LUA_REGISTRYINDEX);
  }
  return plib;
}

/*
** __gc tag method: calls library's `ll_unloadlib' function with the lib
** handle
*/
static int gctm(lua_State *L) {
  void **lib = (void **)luaL_checkudata(L, 1, "_LOADLIB");
  if (*lib) {
    ll_unloadlib(*lib);
  }
  *lib = NULL; /* mark library as closed */
  return 0;
}

static int ll_loadfunc(lua_State *L, const char *path, const char *sym) {
  void **reg = ll_register(L, path);
  if (*reg == NULL) {
    *reg = ll_load(L, path);
  }
  if (*reg == NULL) {
    return ERRLIB; /* unable to load library */
  } else {
    lua_CFunction f = ll_sym(L, *reg, sym);
    if (f == NULL) {
      return ERRFUNC; /* unable to find function */
    }
    lua_pushcfunction(L, f);
    return 0; /* return function */
  }
}

static int ll_loadlib(lua_State *L) {
  const char *path = luaL_checkstring(L, 1);
  const char *init = luaL_checkstring(L, 2);
  int stat = ll_loadfunc(L, path, init);
  if (stat == 0) { /* no errors? */
    return 1;      /* return the loaded function */
  } else {         /* error; error message is on stack top */
    lua_pushnil(L);
    lua_insert(L, -2);
    lua_pushstring(L, (stat == ERRLIB) ? LIB_FAIL : "init");
    return 3; /* return nil, error message, and where */
  }
}

/*
** {======================================================
** 'require' function
** =======================================================
*/

static int readable(const char *filename) {
  FILE *f = fopen(filename, "r"); /* try to open file */
  if (f == NULL) {
    return 0; /* open failed */
  }
  fclose(f);
  return 1;
}

static const char *pushnexttemplate(lua_State *L, const char *path) {
  const char *l;
  while (*path == *LUA_PATHSEP) {
    path++; /* skip separators */
  }
  if (*path == '\0') {
    return nullptr; /* no more templates */
  }
  l = strchr(path, *LUA_PATHSEP); /* find next separator */
  if (l == NULL) {
    l = path + strlen(path);
  }
  lua_pushlstring(L, path, l - path); /* template */
  return l;
}

static const char *findfile(lua_State *L, const char *name, const char *pname) {
  const char *path;
  name = luaL_gsub(L, name, ".", LUA_DIRSEP);
  lua_getfield(L, LUA_ENVIRONINDEX, pname);
  path = lua_tostring(L, -1);
  if (path == NULL) {
    luaL_error(L, "'package.%s' must be a string", pname);
  }
  lua_pushliteral(L, ""); /* error accumulator */
  while ((path = pushnexttemplate(L, path)) != NULL) {
    const char *filename;
    filename = luaL_gsub(L, lua_tostring(L, -1), LUA_PATH_MARK, name);
    lua_remove(L, -2);        /* remove path template */
    if (readable(filename)) { /* does file exist and is readable? */
      return filename;        /* return that file name */
    }
    lua_pushfstring(L, "\n\tno file '%s'", filename);
    lua_remove(L, -2); /* remove file name */
    lua_concat(L, 2);  /* add entry to possible error message */
  }
  return nullptr; /* not found */
}

static void loaderror(lua_State *L, const char *filename) {
  luaL_error(L, "error loading module '%s' from file '%s':\n\t%s",
             lua_tostring(L, 1), filename, lua_tostring(L, -1));
}

static int loader_Lua(lua_State *L) {
  const char *filename;
  const char *name = luaL_checkstring(L, 1);
  filename = findfile(L, name, "path");
  if (filename == NULL) {
    return 1; /* library not found in this path */
  }
  if (luaL_loadfile(L, filename) != 0) {
    loaderror(L, filename);
  }
  return 1; /* library loaded successfully */
}

static const char *mkfuncname(lua_State *L, const char *modname) {
  const char *funcname;
  const char *mark = strchr(modname, *LUA_IGMARK);
  if (mark) {
    modname = mark + 1;
  }
  funcname = luaL_gsub(L, modname, ".", LUA_OFSEP);
  funcname = lua_pushfstring(L, POF "%s", funcname);
  lua_remove(L, -2); /* remove 'gsub' result */
  return funcname;
}

static int loader_C(lua_State *L) {
  const char *funcname;
  const char *name = luaL_checkstring(L, 1);
  const char *filename = findfile(L, name, "cpath");
  if (filename == NULL) {
    return 1; /* library not found in this path */
  }
  funcname = mkfuncname(L, name);
  if (ll_loadfunc(L, filename, funcname) != 0) {
    loaderror(L, filename);
  }
  return 1; /* library loaded successfully */
}

static int loader_Croot(lua_State *L) {
  const char *funcname;
  const char *filename;
  const char *name = luaL_checkstring(L, 1);
  const char *p = strchr(name, '.');
  int stat;
  if (p == NULL) {
    return 0; /* is root */
  }
  lua_pushlstring(L, name, p - name);
  filename = findfile(L, lua_tostring(L, -1), "cpath");
  if (filename == NULL) {
    return 1; /* root not found */
  }
  funcname = mkfuncname(L, name);
  if ((stat = ll_loadfunc(L, filename, funcname)) != 0) {
    if (stat != ERRFUNC) {
      loaderror(L, filename); /* real error */
    }
    lua_pushfstring(L, "\n\tno module '%s' in file '%s'", name, filename);
    return 1; /* function not found */
  }
  return 1;
}

static int loader_preload(lua_State *L) {
  const char *name = luaL_checkstring(L, 1);
  lua_getfield(L, LUA_ENVIRONINDEX, "preload");
  if (!lua_istable(L, -1)) {
    luaL_error(L, "'package.preload' must be a table");
  }
  lua_getfield(L, -1, name);
  if (lua_isnil(L, -1)) { /* not found? */
    lua_pushfstring(L, "\n\tno field package.preload['%s']", name);
  }
  return 1;
}

static const int sentinel_ = 0;
#define sentinel ((void *)&sentinel_)

static int ll_require(lua_State *L) {
  const char *name = luaL_checkstring(L, 1);
  int i;
  lua_settop(L, 1); /* _LOADED table will be at index 2 */
  lua_getfield(L, LUA_REGISTRYINDEX, "_LOADED");
  lua_getfield(L, 2, name);
  if (lua_toboolean(L, -1)) {                /* is it there? */
    if (lua_touserdata(L, -1) == sentinel) { /* check loops */
      luaL_error(L, "loop or previous error loading module '%s'", name);
    }
    return 1; /* package is already loaded */
  }
  /* else must load it; iterate over available loaders */
  lua_getfield(L, LUA_ENVIRONINDEX, "loaders");
  if (!lua_istable(L, -1)) {
    luaL_error(L, "'package.loaders' must be a table");
  }
  lua_pushliteral(L, ""); /* error message accumulator */
  for (i = 1;; i++) {
    lua_rawgeti(L, -2, i); /* get a loader */
    if (lua_isnil(L, -1)) {
      luaL_error(L, "module '%s' not found:%s", name, lua_tostring(L, -2));
    }
    lua_pushstring(L, name);
    lua_call(L, 1, 1);                /* call it */
    if (lua_isfunction(L, -1)) {      /* did it find module? */
      break;                          /* module loaded successfully */
    } else if (lua_isstring(L, -1)) { /* loader returned error message? */
      lua_concat(L, 2);               /* accumulate it */
    } else {
      lua_pop(L, 1);
    }
  }
  lua_pushlightuserdata(L, sentinel);
  lua_setfield(L, 2, name);   /* _LOADED[name] = sentinel */
  lua_pushstring(L, name);    /* pass name as argument to module */
  lua_call(L, 1, 1);          /* run loaded module */
  if (!lua_isnil(L, -1)) {    /* non-nil return? */
    lua_setfield(L, 2, name); /* _LOADED[name] = returned value */
  }
  lua_getfield(L, 2, name);
  if (lua_touserdata(L, -1) == sentinel) { /* module did not set a value? */
    lua_pushboolean(L, 1);                 /* use true as result */
    lua_pushvalue(L, -1);                  /* extra copy to be returned */
    lua_setfield(L, 2, name);              /* _LOADED[name] = true */
  }
  return 1;
}

/* }====================================================== */

/*
** {======================================================
** 'module' function
** =======================================================
*/

static void setfenv(lua_State *L) {
  lua_Debug ar;
  if (lua_getstack(L, 1, &ar) == 0 ||
      lua_getinfo(L, "f", &ar) == 0 || /* get calling function */
      lua_iscfunction(L, -1)) {
    luaL_error(L, "'module' not called from a Lua function");
  }
  lua_pushvalue(L, -2);
  lua_setfenv(L, -2);
  lua_pop(L, 1);
}

static void dooptions(lua_State *L, int n) {
  int i;
  for (i = 2; i <= n; i++) {
    lua_pushvalue(L, i);  /* get option (a function) */
    lua_pushvalue(L, -2); /* module */
    lua_call(L, 1, 0);
  }
}

static void modinit(lua_State *L, const char *modname) {
  const char *dot;
  lua_pushvalue(L, -1);
  lua_setfield(L, -2, "_M"); /* module._M = module */
  lua_pushstring(L, modname);
  lua_setfield(L, -2, "_NAME");
  dot = strrchr(modname, '.'); /* look for last dot in module name */
  if (dot == NULL) {
    dot = modname;
  } else {
    dot++;
  }
  /* set _PACKAGE as package name (full module name minus last part) */
  lua_pushlstring(L, modname, dot - modname);
  lua_setfield(L, -2, "_PACKAGE");
}

static int ll_module(lua_State *L) {
  const char *modname = luaL_checkstring(L, 1);
  int loaded = lua_gettop(L) + 1; /* index of _LOADED table */
  lua_getfield(L, LUA_REGISTRYINDEX, "_LOADED");
  lua_getfield(L, loaded, modname); /* get _LOADED[modname] */
  if (!lua_istable(L, -1)) {        /* not found? */
    lua_pop(L, 1);                  /* remove previous result */
    /* try global variable (and create one if it does not exist) */
    if (luaL_findtable(L, LUA_GLOBALSINDEX, modname, 1) != NULL) {
      return luaL_error(L, "name conflict for module '%s'", modname);
    }
    lua_pushvalue(L, -1);
    lua_setfield(L, loaded, modname); /* _LOADED[modname] = new table */
  }
  /* check whether table already has a _NAME field */
  lua_getfield(L, -1, "_NAME");
  if (!lua_isnil(L, -1)) { /* is table an initialized module? */
    lua_pop(L, 1);
  } else { /* no; initialize it */
    lua_pop(L, 1);
    modinit(L, modname);
  }
  lua_pushvalue(L, -1);
  setfenv(L);
  dooptions(L, loaded - 1);
  return 0;
}

static int ll_seeall(lua_State *L) {
  luaL_checktype(L, 1, LUA_TYPE_TABLE);
  if (!lua_getmetatable(L, 1)) {
    lua_createtable(L, 0, 1); /* create new metatable */
    lua_pushvalue(L, -1);
    lua_setmetatable(L, 1);
  }
  lua_pushvalue(L, LUA_GLOBALSINDEX);
  lua_setfield(L, -2, "__index"); /* mt.__index = _G */
  return 0;
}

/* }====================================================== */

/* auxiliary mark (for internal use) */
#define AUXMARK "\1"

static void setpath(lua_State *L, const char *fieldname, const char *envname,
                    const char *def) {
  const char *path = getenv(envname);
  if (path == NULL) {       /* no environment variable? */
    lua_pushstring(L, def); /* use default */
  } else {
    /* replace ";;" by ";AUXMARK;" and then AUXMARK by default path */
    path = luaL_gsub(L, path, LUA_PATHSEP LUA_PATHSEP,
                     LUA_PATHSEP AUXMARK LUA_PATHSEP);
    luaL_gsub(L, path, AUXMARK, def);
    lua_remove(L, -2);
  }
  setprogdir(L);
  lua_setfield(L, -2, fieldname);
}

static const luaL_Reg pk_funcs[] = {
    {"loadlib", ll_loadlib},
    {"seeall", ll_seeall},
    {nullptr, nullptr},
};

static const luaL_Reg ll_funcs[] = {
    {"module", ll_module},
    {"require", ll_require},
    {nullptr, nullptr},
};

static const lua_CFunction loaders[] = {
    loader_preload, loader_Lua, loader_C, loader_Croot, nullptr,
};

LUALIB_API int luaopen_package(lua_State *L) {
  int i;
  /* create new type _LOADLIB */
  luaL_newmetatable(L, "_LOADLIB");
  lua_pushcfunction(L, gctm);
  lua_setfield(L, -2, "__gc");
  /* create `package' table */
  luaL_register(L, "package", pk_funcs);

  lua_pushvalue(L, -1);
  lua_replace(L, LUA_ENVIRONINDEX);
  /* create `loaders' table */
  lua_createtable(L, sizeof(loaders) / sizeof(loaders[0]) - 1, 0);
  /* fill it with pre-defined loaders */
  for (i = 0; loaders[i] != NULL; i++) {
    lua_pushcfunction(L, loaders[i]);
    lua_rawseti(L, -2, i + 1);
  }
  lua_setfield(L, -2, "loaders"); /* put it in field `loaders' */
  setpath(L, "path", LUA_PATH, LUA_PATH_DEFAULT);    /* set field `path' */
  setpath(L, "cpath", LUA_CPATH, LUA_CPATH_DEFAULT); /* set field `cpath' */
  /* store config information */
  lua_pushliteral(L, LUA_DIRSEP "\n" LUA_PATHSEP "\n" LUA_PATH_MARK
                                "\n" LUA_EXECDIR "\n" LUA_IGMARK);
  lua_setfield(L, -2, "config");
  /* set field `loaded' */
  luaL_findtable(L, LUA_REGISTRYINDEX, "_LOADED", 2);
  lua_setfield(L, -2, "loaded");
  /* set field `preload' */
  lua_newtable(L);
  lua_setfield(L, -2, "preload");
  lua_pushvalue(L, LUA_GLOBALSINDEX);
  luaL_register(L, nullptr, ll_funcs); /* open lib into global table */
  lua_pop(L, 1);
  return 1; /* return 'package' table */
}
