// Configuration file for Lua.

#pragma once

#include <limits.h>
#include <stddef.h>
#include <stdint.h>

/*
** ==================================================================
** Search for "@@" to find all configurable definitions.
** ===================================================================
*/

/*
@@ LUA_PATH and LUA_CPATH are the names of the environment variables that
@* Lua check to set its paths.
@@ LUA_INIT is the name of the environment variable that Lua
@* checks for initialization code.
** CHANGE them if you want different names.
*/
#define LUA_PATH "LUA_PATH"
#define LUA_CPATH "LUA_CPATH"
#define LUA_INIT "LUA_INIT"

/*
@@ LUA_PATH_DEFAULT is the default path that Lua uses to look for
@* Lua LIBS.
@@ LUA_CPATH_DEFAULT is the default path that Lua uses to look for
@* C LIBS.
** CHANGE them if your machine has a non-conventional directory
** hierarchy or if you want to install your LIBS in
** non-conventional directories.
*/
#define LUA_ROOT "/usr/local/"
#define LUA_LDIR LUA_ROOT "share/lua/5.1/"
#define LUA_CDIR LUA_ROOT "lib/lua/5.1/"
#define LUA_PATH_DEFAULT                                                       \
  "./?.lua;" LUA_LDIR "?.lua;" LUA_LDIR "?/init.lua;" LUA_CDIR                 \
  "?.lua;" LUA_CDIR "?/init.lua"
#define LUA_CPATH_DEFAULT "./?.so;" LUA_CDIR "?.so;" LUA_CDIR "loadall.so"

/*
@@ LUA_DIRSEP is the directory separator (for submodules).
** CHANGE it if your machine does not use "/" as the directory separator
** and is not Windows. (On Windows Lua automatically uses "\".)
*/
#define LUA_DIRSEP "/"

/*
@@ LUA_PATHSEP is the character that separates templates in a path.
@@ LUA_PATH_MARK is the string that marks the substitution points in a
@* template.
@@ LUA_EXECDIR in a Windows path is replaced by the executable's
@* directory.
@@ LUA_IGMARK is a mark to ignore all before it when bulding the
@* luaopen_ function name.
** CHANGE them if for some reason your system cannot use those
** characters. (E.g., if one of those characters is a common character
** in file/directory names.) Probably you do not need to change them.
*/
#define LUA_PATHSEP ";"
#define LUA_PATH_MARK "?"
#define LUA_EXECDIR "!"
#define LUA_IGMARK "-"

/*
@@ LUA_INTEGER is the integral type used by lua_pushinteger/lua_tointeger.
** CHANGE that if ptrdiff_t is not adequate on your machine. (On most
** machines, ptrdiff_t gives a good choice between int or long.)
*/
#define LUA_INTEGER ptrdiff_t

/*
@@ LUA_API is a mark for all core API functions.
@@ LUALIB_API is a mark for all standard library functions.
** CHANGE them if you need to define those functions in some special way.
*/
#define LUA_API extern

/* more often than not the libs go together with the core */
#define LUALIB_API LUA_API

/*
@@ LUAI_FUNC is a mark for all extern functions that are not to be
@* exported to outside modules.
@@ LUAI_DATA is a mark for all extern (const) variables that are not to
@* be exported to outside modules.
** CHANGE them if you need to mark them in some special way. Elf/gcc
** (versions 3.2 and later) mark them as "hidden" to optimize access
** when Lua is compiled as a shared library.
*/
#define LUAI_FUNC extern
#define LUAI_DATA extern

/*
@@ LUA_IDSIZE gives the maximum size for the description of the source
@* of a function in debug information.
** CHANGE it if you want a different size.
*/
#define LUA_IDSIZE 60

/*
@@ LUAI_GCPAUSE defines the default pause between garbage-collector cycles
@* as a percentage.
** CHANGE it if you want the GC to run faster or slower (higher values
** mean larger pauses which mean slower collection.) You can also change
** this value dynamically.
*/
#define LUAI_GCPAUSE 200 /* 200% (wait memory to double before next GC) */

/*
@@ LUAI_GCMUL defines the default speed of garbage collection relative to
@* memory allocation as a percentage.
** CHANGE it if you want to change the granularity of the garbage
** collection. (Higher values mean coarser collections. 0 represents
** infinity, where each step performs a full collection.) You can also
** change this value dynamically.
*/
#define LUAI_GCMUL 200 /* GC runs 'twice the speed' of memory allocation */

/*
@@ LUAI_MAXCALLS limits the number of nested calls.
** CHANGE it if you need really deep recursive calls. This limit is
** arbitrary; its only purpose is to stop infinite recursion before
** exhausting memory.
*/
#define LUAI_MAXCALLS 20000

/*
@@ LUAI_MAXCSTACK limits the number of Lua stack slots that a C function
@* can use.
** CHANGE it if you need lots of (Lua) stack space for your C
** functions. This limit is arbitrary; its only purpose is to stop C
** functions to consume unlimited stack space. (must be smaller than
** -LUA_REGISTRYINDEX)
*/
#define LUAI_MAXCSTACK 8000

/*
** {==================================================================
** CHANGE (to smaller values) the following definitions if your system
** has a small C stack. (Or you may want to change them to larger
** values if your system has a large C stack and these limits are
** too rigid for you.) Some of these constants control the size of
** stack-allocated arrays used by the compiler or the interpreter, while
** others limit the maximum number of recursive calls that the compiler
** or the interpreter can perform. Values too large may cause a C stack
** overflow for some forms of deep constructs.
** ===================================================================
*/

/*
@@ LUAI_MAX_C_CALLS is the maximum depth for nested C calls (short) and
@* syntactical nested non-terminals in a program.
*/
#define LUAI_MAX_C_CALLS 200

/*
@@ LUAI_MAX_VARS is the maximum number of local variables per function
@* (must be smaller than 250).
*/
#define LUAI_MAX_VARS 200

/*
@@ LUAI_MAX_UPVALUES is the maximum number of upvalues per function
@* (must be smaller than 250).
*/
#define LUAI_MAX_UPVALUES 60

/*
@@ LUAL_BUFFER_SIZE is the buffer size used by the lauxlib buffer system.
*/
#define LUAL_BUFFER_SIZE BUFSIZ

/* }================================================================== */

/*
@@ LUA_NUMBER_SCAN is the format for reading numbers.
@@ LUA_NUMBER_FMT is the format for writing numbers.
@@ lua_number2str converts a number to a string.
@@ LUAI_MAXNUMBER2STR is maximum size of previous conversion.
@@ lua_str2number converts a string to a number.
*/
#define LUA_NUMBER_SCAN "%lf"
#define LUA_NUMBER_FMT "%.14g"
#define LUAI_MAXNUMBER2STR 32 /* 16 digits, sign, point, and \0 */
#define lua_str2number(s, p) strtod((s), (p))

/* }================================================================== */

/*
@@ LUA_MAXCAPTURES is the maximum number of captures that a pattern
@* can do during pattern-matching.
** CHANGE it if you need more captures. This limit is arbitrary.
*/
#define LUA_MAXCAPTURES 32

/*
@@ lua_popen spawns a new process connected to the current one through
@* the file streams.
** CHANGE it if you have a way to implement it in your system.
*/
#define lua_popen(L, c, m) ((void)L, fflush(nullptr), popen(c, m))
#define lua_pclose(L, file) ((void)L, (pclose(file) != -1))

/*
@@ LUAI_EXTRASPACE allows you to add user-specific ud in a lua_State
@* (the ud goes just *before* the lua_State pointer).
** CHANGE (define) this if you really need that. This value must be
** a multiple of the maximum alignment required for your machine.
*/
#define LUAI_EXTRASPACE 0

/*
@@ luai_userstate* allow user-specific actions on threads.
** CHANGE them if you defined LUAI_EXTRASPACE and need to do something
** extra when a thread is created/deleted/resumed/yielded.
*/
#define luai_userstateopen(L) ((void)L)
#define luai_userstateclose(L) ((void)L)
#define luai_userstatethread(L, L1) ((void)L)
#define luai_userstatefree(L) ((void)L)
#define luai_userstateresume(L, n) ((void)L)
#define luai_userstateyield(L, n) ((void)L)

/* =================================================================== */

/*
** Local configuration. You can use this space to add your redefinitions
** without modifying the main part of the file.
*/
