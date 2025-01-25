/* Lua compiler (saves bytecodes to files; also list bytecodes). */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"
#include "std.h"

#include "closure.h"
#include "intern.h"
#include "load.h"
#include "memory.h"
#include "object.h"
#include "opcodes.h"
#include "stack.h"

#define PROGNAME "luac"        /* default program name */
#define OUTPUT PROGNAME ".out" /* default output file */

static bool listing = 0;                /* list bytecodes? */
static bool dumping = 1;                /* dump bytecodes? */
static bool stripping = 0;              /* strip debug information? */
static char Output[] = {OUTPUT};        /* default output file name */
static const char *output = Output;     /* actual output file name */
static const char *progname = PROGNAME; /* actual program name */

static void fatal(const char *message) {
  fprintf(stderr, "%s: %s\n", progname, message);
  exit(EXIT_FAILURE);
}

static void cannot(const char *what) {
  fprintf(stderr, "%s: cannot %s %s: %s\n", progname, what, output,
          strerror(errno));
  exit(EXIT_FAILURE);
}

static void usage(const char *message) {
  if (*message == '-') {
    fprintf(stderr, "%s: unrecognized option '%s'\n", progname, message);
  } else {
    fprintf(stderr, "%s: %s\n", progname, message);
  }
  fprintf(stderr,
          "usage: %s [options] [filenames].\n"
          "Available options are:\n"
          "  -        process stdin\n"
          "  -l       list\n"
          "  -o name  output to file 'name' (default is \"%s\")\n"
          "  -p       parse only\n"
          "  -s       strip debug information\n"
          "  -v       show version information\n"
          "  --       stop handling options\n",
          progname, Output);
  exit(EXIT_FAILURE);
}

#define IS(s) (strcmp(argv[i], s) == 0)

static int doargs(int argc, const char *argv[]) {
  int version = 0;
  if (argv[0] != nullptr && *argv[0] != 0) {
    progname = argv[0];
  }
  int i = 1;
  for (; i < argc; i++) {
    if (*argv[i] != '-') { /* end of options; keep it */
      break;
    } else if (IS("--")) { /* end of options; skip it */
      ++i;
      if (version) {
        ++version;
      }
      break;
    } else if (IS("-")) { /* end of options; use stdin */
      break;
    } else if (IS("-l")) { /* list */
      listing = true;
    } else if (IS("-o")) { /* output file */
      output = argv[++i];
      if (output == nullptr || *output == 0) {
        usage("'-o' needs argument");
      }
      if (IS("-")) {
        output = nullptr;
      }
    } else if (IS("-p")) { /* parse only */
      dumping = false;
    } else if (IS("-s")) { /* strip debug information */
      stripping = true;
    } else if (IS("-v")) { /* show version */
      ++version;
    } else { /* unknown option */
      usage(argv[i]);
    }
  }
  if (i == argc && (listing || !dumping)) {
    dumping = false;
    argv[--i] = Output;
  }
  if (version) {
    printf("%s  %s\n", LUA_RELEASE, LUA_COPYRIGHT);
    if (version == argc - 1) {
      exit(EXIT_SUCCESS);
    }
  }
  return i;
}

#define toproto(L, i) (CLOSURE_VALUE(L->top + (i))->l.p)

static const Prototype *combine(lua_State *L, int n) {
  if (n == 1) {
    return toproto(L, -1);
  }
  Prototype *f = Prototype_new(L);
  SET_PROTO_TO_STACK(L, L->top, f);
  incr_top(L);
  f->source = String_createLiteral(L, "=(" PROGNAME ")");
  f->maxStackSize = 1;
  int pc = 2 * n + 1;
  f->code = Mem_newVec(L, pc, Instruction);
  f->codeSize = pc;
  f->inners = Mem_newVec(L, n, Prototype *);
  f->innersSize = n;
  pc = 0;
  for (int i = 0; i < n; i++) {
    f->inners[i] = toproto(L, i - n - 1);
    f->code[pc++] = CREATE_ABx(OP_CLOSURE, 0, i);
    f->code[pc++] = CREATE_ABC(OP_CALL, 0, 1, 1);
  }
  f->code[pc] = CREATE_ABC(OP_RETURN, 0, 1, 0);
  return f;
}

static int writer(lua_State *, const void *p, size_t size, void *u) {
  return (fwrite(p, size, 1, (FILE *)u) != 1) && (size != 0);
}

struct Compiler {
  int argc;
  const char **argv;
};

static int Compiler_main(lua_State *L) {
  struct Compiler *s = lua_touserdata(L, 1);
  int argc = s->argc;
  const char **argv = s->argv;
  if (!lua_checkstack(L, argc)) {
    fatal("too many input files");
  }
  for (int i = 0; i < argc; i++) {
    const char *filename = IS("-") ? nullptr : argv[i];
    if (luaL_loadfile(L, filename) != 0) {
      fatal(lua_tostring(L, -1));
    }
  }
  const Prototype *f = combine(L, argc);
  if (listing) {
    luaU_print(f, listing);
  }
  if (dumping) {
    FILE *D = (output == nullptr) ? stdout : fopen(output, "wb");
    if (D == nullptr) {
      cannot("open");
    }
    lua_lock(L);
    luaU_dump(L, f, writer, D, stripping);
    lua_unlock(L);
    if (ferror(D)) {
      cannot("write");
    }
    if (fclose(D)) {
      cannot("close");
    }
  }
  return 0;
}

int main(int argc, const char *argv[]) {
  int i = doargs(argc, argv);
  argc -= i;
  argv += i;
  if (argc <= 0) {
    usage("no input files given");
  }
  lua_State *L = lua_open();
  if (L == nullptr) {
    fatal("not enough memory for state");
  }
  struct Compiler compiler = {.argc = argc, .argv = argv};
  if (lua_cpcall(L, Compiler_main, &compiler) != 0) {
    fatal(lua_tostring(L, -1));
  }
  lua_close(L);
  return EXIT_SUCCESS;
}
