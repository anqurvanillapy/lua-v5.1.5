/* Stack and Call structure of Lua. */

#include <setjmp.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"

#include "buffer.h"
#include "closure.h"
#include "debug.h"
#include "gc.h"
#include "intern.h"
#include "load.h"
#include "memory.h"
#include "object.h"
#include "opcodes.h"
#include "parser.h"
#include "stack.h"
#include "state.h"
#include "table.h"
#include "tag.h"
#include "vm.h"

/*
** {======================================================
** Error-recovery functions
** =======================================================
*/

/* chain list of long jump buffers */
struct lua_longjmp {
  struct lua_longjmp *previous;
  luai_jmpbuf b;
  volatile int status; /* error code */
};

void luaD_seterrorobj(lua_State *L, int errcode, StackIndex oldtop) {
  switch (errcode) {
  case LUA_ERRMEM: {
    SET_STRING_TO_STACK(L, oldtop, String_createLiteral(L, MEMERRMSG));
    break;
  }
  case LUA_ERRERR: {
    SET_STRING_TO_STACK(L, oldtop,
                        String_createLiteral(L, "error in error handling"));
    break;
  }
  case LUA_ERRSYNTAX:
  case LUA_ERRRUN: {
    SET_OBJECT_TO_SAME_STACK(L, oldtop,
                             L->top - 1); /* error message on current top */
    break;
  }
  default:
    break;
  }
  L->top = oldtop + 1;
}

static void restore_stack_limit(lua_State *L) {
  assert(L->stackLast - L->stack == L->stackSize - EXTRA_STACK - 1);
  if (L->ciSize > LUAI_MAXCALLS) { /* there was an overflow? */
    int inuse = (int)(L->ci - L->baseCI);
    if (inuse + 1 < LUAI_MAXCALLS) { /* can `undo' overflow? */
      luaD_reallocCI(L, LUAI_MAXCALLS);
    }
  }
}

static void resetstack(lua_State *L, int status) {
  L->ci = L->baseCI;
  L->base = L->ci->base;
  luaF_close(L, L->base); /* close eventual pending closures */
  luaD_seterrorobj(L, status, L->base);
  L->nestedCCallsNum = L->nestedCCallsBaseNum;
  L->allowHook = 1;
  restore_stack_limit(L);
  L->errFunc = 0;
  L->errorJmp = nullptr;
}

[[noreturn]] void luaD_throw(lua_State *L, int errcode) {
  if (L->errorJmp) {
    L->errorJmp->status = errcode;
    LUAI_THROW(L, L->errorJmp);
  } else {
    L->status = (uint8_t)errcode;
    if (G(L)->panic) {
      resetstack(L, errcode);
      lua_unlock(L);
      G(L)->panic(L);
    }
    exit(EXIT_FAILURE);
  }
}

int luaD_rawrunprotected(lua_State *L, Pfunc f, void *ud) {
  struct lua_longjmp lj;
  lj.status = 0;
  lj.previous = L->errorJmp; /* chain new error handler */
  L->errorJmp = &lj;
  LUAI_TRY(L, &lj, (*f)(L, ud););
  L->errorJmp = lj.previous; /* restore old error handler */
  return lj.status;
}

/* }====================================================== */

static void correctstack(lua_State *L, Value *oldstack) {
  CallInfo *ci;
  GCObject *up;
  L->top = (L->top - oldstack) + L->stack;
  for (up = L->openUpval; up != NULL; up = up->gch.next) {
    gco2uv(up)->v = (gco2uv(up)->v - oldstack) + L->stack;
  }
  for (ci = L->baseCI; ci <= L->ci; ci++) {
    ci->top = (ci->top - oldstack) + L->stack;
    ci->base = (ci->base - oldstack) + L->stack;
    ci->func = (ci->func - oldstack) + L->stack;
  }
  L->base = (L->base - oldstack) + L->stack;
}

void luaD_reallocstack(lua_State *L, int newsize) {
  Value *oldstack = L->stack;
  int realsize = newsize + 1 + EXTRA_STACK;
  assert(L->stackLast - L->stack == L->stackSize - EXTRA_STACK - 1);
  Mem_reallocVec(L, L->stack, L->stackSize, realsize, Value);
  L->stackSize = realsize;
  L->stackLast = L->stack + newsize;
  correctstack(L, oldstack);
}

void luaD_reallocCI(lua_State *L, int newsize) {
  CallInfo *oldci = L->baseCI;
  Mem_reallocVec(L, L->baseCI, L->ciSize, newsize, CallInfo);
  L->ciSize = newsize;
  L->ci = (L->ci - oldci) + L->baseCI;
  L->endCI = L->baseCI + L->ciSize - 1;
}

void luaD_growstack(lua_State *L, int n) {
  if (n <= L->stackSize) { /* double size is enough? */
    luaD_reallocstack(L, 2 * L->stackSize);
  } else {
    luaD_reallocstack(L, L->stackSize + n);
  }
}

static CallInfo *growCI(lua_State *L) {
  if (L->ciSize > LUAI_MAXCALLS) { /* overflow while handling overflow? */
    luaD_throw(L, LUA_ERRERR);
  } else {
    luaD_reallocCI(L, 2 * L->ciSize);
    if (L->ciSize > LUAI_MAXCALLS) {
      luaG_runerror(L, "stack overflow");
    }
  }
  return ++L->ci;
}

void luaD_callhook(lua_State *L, int event, int line) {
  lua_Hook hook = L->hook;
  if (hook && L->allowHook) {
    ptrdiff_t top = savestack(L, L->top);
    ptrdiff_t ci_top = savestack(L, L->ci->top);
    lua_Debug ar;
    ar.event = event;
    ar.currentline = line;
    if (event == LUA_HOOKTAILRET) {
      ar.i_ci = 0; /* tail call; no debug information about it */
    } else {
      ar.i_ci = (int)(L->ci - L->baseCI);
    }
    luaD_checkstack(L, LUA_MIN_STACK); /* ensure minimum stack size */
    L->ci->top = L->top + LUA_MIN_STACK;
    assert(L->ci->top <= L->stackLast);
    L->allowHook = 0; /* cannot call hooks inside a hook */
    lua_unlock(L);
    (*hook)(L, &ar);
    lua_lock(L);
    assert(!L->allowHook);
    L->allowHook = 1;
    L->ci->top = restorestack(L, ci_top);
    L->top = restorestack(L, top);
  }
}

static StackIndex adjust_varargs(lua_State *L, Prototype *p, int actual) {
  int i;
  int nfixargs = p->paramsNum;
  Table *htab = NULL;
  StackIndex base, fixed;
  for (; actual < nfixargs; ++actual) {
    SET_NIL(L->top++);
  }

  // Compatible with old-style variadic arguments.
  if (p->varargMode & VARARG_NEEDS_ARG) {
    int nvar = actual - nfixargs; /* number of extra arguments */
    assert(p->varargMode & VARARG_HAS_ARG);
    luaC_checkGC(L);
    luaD_checkstack(L, p->maxStackSize);
    htab = Table_new(L, nvar, 1); /* create `arg' table */
    for (i = 0; i < nvar; i++) {
      /* put extra arguments into 'arg' table */
      SET_OBJECT_TO_NEW(L, Table_insertInteger(L, htab, i + 1),
                        L->top - nvar + i);
    }
    /* store counter in field `n' */
    SET_NUMBER(Table_insertString(L, htab, String_createLiteral(L, "n")),
               (double)nvar);
  }

  /* move fixed parameters to final position */
  fixed = L->top - actual; /* first fixed argument */
  base = L->top;           /* final position of first argument */
  for (i = 0; i < nfixargs; i++) {
    SET_OBJECT_TO_SAME_STACK(L, L->top++, fixed + i);
    SET_NIL(fixed + i);
  }
  /* add `arg' parameter */
  if (htab) {
    SET_TABLE(L, L->top++, htab);
    assert(iswhite(LuaObjectToGCObject(htab)));
  }
  return base;
}

static StackIndex tryfuncTM(lua_State *L, StackIndex func) {
  const Value *tm = luaT_gettmbyobj(L, func, TM_CALL);
  StackIndex p;
  ptrdiff_t funcr = savestack(L, func);
  if (!IS_TYPE_FUNCTION(tm)) {
    luaG_typeerror(L, func, "call");
  }
  /* Open a hole inside the stack at `func' */
  for (p = L->top; p > func; p--)
    SET_OBJECT_TO_SAME_STACK(L, p, p - 1);
  incr_top(L);
  func = restorestack(L, funcr); /* previous call may change stack */
  SET_OBJECT_TO_STACK(L, func,
                      tm); /* tag method is the new function to be called */
  return func;
}

#define inc_ci(L)                                                              \
  ((L->ci == L->endCI)                                                         \
       ? growCI(L)                                                             \
       : (condhardstacktests(luaD_reallocCI(L, L->ciSize)), ++L->ci))

int luaD_precall(lua_State *L, StackIndex func, int nresults) {
  LClosure *cl;
  ptrdiff_t funcr;
  if (!IS_TYPE_FUNCTION(func)) { /* `func' is not a function? */
    func = tryfuncTM(L, func);   /* check the `function' tag method */
  }
  funcr = savestack(L, func);
  cl = &CLOSURE_VALUE(func)->l;
  L->ci->savedpc = L->savedPC;
  if (!cl->header.isC) { /* Lua function? prepare its call */
    CallInfo *ci;
    StackIndex st, base;
    Prototype *p = cl->p;
    luaD_checkstack(L, p->maxStackSize);
    func = restorestack(L, funcr);
    if (!p->varargMode) { /* no varargs? */
      base = func + 1;
      if (L->top > base + p->paramsNum) {
        L->top = base + p->paramsNum;
      }
    } else { /* vararg function */
      int nargs = (int)(L->top - func) - 1;
      base = adjust_varargs(L, p, nargs);
      func = restorestack(L, funcr); /* previous call may change the stack */
    }
    ci = inc_ci(L); /* now `enter' new function */
    ci->func = func;
    L->base = ci->base = base;
    ci->top = L->base + p->maxStackSize;
    assert(ci->top <= L->stackLast);
    L->savedPC = p->code; /* starting point */
    ci->tailcalls = 0;
    ci->nresults = nresults;
    for (st = L->top; st < ci->top; st++) {
      SET_NIL(st);
    }
    L->top = ci->top;
    if (L->hookMask & LUA_MASKCALL) {
      L->savedPC++; /* hooks assume 'pc' is already incremented */
      luaD_callhook(L, LUA_HOOKCALL, -1);
      L->savedPC--; /* correct 'pc' */
    }
    return PCRLUA;
  } else { /* if is a C function, call it */
    CallInfo *ci;
    int n;
    luaD_checkstack(L, LUA_MIN_STACK); /* ensure minimum stack size */
    ci = inc_ci(L);                    /* now `enter' new function */
    ci->func = restorestack(L, funcr);
    L->base = ci->base = ci->func + 1;
    ci->top = L->top + LUA_MIN_STACK;
    assert(ci->top <= L->stackLast);
    ci->nresults = nresults;
    if (L->hookMask & LUA_MASKCALL) {
      luaD_callhook(L, LUA_HOOKCALL, -1);
    }
    lua_unlock(L);
    n = (*CUR_FUNC(L)->c.f)(L); /* do the actual call */
    lua_lock(L);
    if (n < 0) { /* yielding? */
      return PCRYIELD;
    } else {
      luaD_poscall(L, L->top - n);
      return PCRC;
    }
  }
}

static StackIndex callrethooks(lua_State *L, StackIndex firstResult) {
  ptrdiff_t fr = savestack(L, firstResult); /* next call may change stack */
  luaD_callhook(L, LUA_HOOKRET, -1);
  if (f_isLua(L->ci)) { /* Lua function? */
    while ((L->hookMask & LUA_MASKRET) && L->ci->tailcalls--) { /* tail calls */
      luaD_callhook(L, LUA_HOOKTAILRET, -1);
    }
  }
  return restorestack(L, fr);
}

int luaD_poscall(lua_State *L, StackIndex firstResult) {
  StackIndex res;
  int wanted, i;
  CallInfo *ci;
  if (L->hookMask & LUA_MASKRET) {
    firstResult = callrethooks(L, firstResult);
  }
  ci = L->ci--;
  res = ci->func; /* res == final position of 1st result */
  wanted = ci->nresults;
  L->base = (ci - 1)->base;       /* restore base */
  L->savedPC = (ci - 1)->savedpc; /* restore savedPC */
  /* move results to correct place */
  for (i = wanted; i != 0 && firstResult < L->top; i--)
    SET_OBJECT_TO_SAME_STACK(L, res++, firstResult++);
  while (i-- > 0) {
    SET_NIL(res++);
  }
  L->top = res;
  return (wanted - LUA_MULTRET); /* 0 iff wanted == LUA_MULTRET */
}

/*
** Call a function (C or Lua). The function to be called is at *func.
** The arguments are on the stack, right after the function.
** When returns, all the results are on the stack, starting at the original
** function position.
*/
void luaD_call(lua_State *L, StackIndex func, int nResults) {
  if (++L->nestedCCallsNum >= LUAI_MAX_C_CALLS) {
    if (L->nestedCCallsNum == LUAI_MAX_C_CALLS) {
      luaG_runerror(L, "C stack overflow");
    } else if (L->nestedCCallsNum >=
               (LUAI_MAX_C_CALLS + (LUAI_MAX_C_CALLS >> 3))) {
      luaD_throw(L, LUA_ERRERR); /* error while handing stack error */
    }
  }
  if (luaD_precall(L, func, nResults) == PCRLUA) { /* is a Lua function? */
    luaV_execute(L, 1);                            /* call it */
  }
  L->nestedCCallsNum--;
  luaC_checkGC(L);
}

static void resume(lua_State *L, void *ud) {
  StackIndex firstArg = cast(StackIndex, ud);
  CallInfo *ci = L->ci;
  if (L->status == LUA_RUNNING) {
    assert(ci == L->baseCI && firstArg > L->base);
    if (luaD_precall(L, firstArg - 1, LUA_MULTRET) != PCRLUA) {
      return;
    }
  } else { /* resuming from previous yield */
    assert(L->status == LUA_YIELD);
    L->status = LUA_RUNNING;
    if (!f_isLua(ci)) { /* `common' yield? */
      /* finish interrupted execution of `OP_CALL' */
      assert(GET_OPCODE(*((ci - 1)->savedpc - 1)) == OP_CALL ||
             GET_OPCODE(*((ci - 1)->savedpc - 1)) == OP_TAILCALL);
      if (luaD_poscall(L, firstArg)) { /* complete it... */
        L->top = L->ci->top; /* and correct top if not multiple results */
      }
    } else { /* yielded inside a hook: just continue its execution */
      L->base = L->ci->base;
    }
  }
  luaV_execute(L, (int)(L->ci - L->baseCI));
}

static int resume_error(lua_State *L, const char *msg) {
  L->top = L->ci->base;
  SET_STRING_TO_STACK(L, L->top, String_create(L, msg));
  incr_top(L);
  lua_unlock(L);
  return LUA_ERRRUN;
}

LUA_API int lua_resume(lua_State *L, int nargs) {
  int status;
  lua_lock(L);
  if (L->status != LUA_YIELD && (L->status != 0 || L->ci != L->baseCI)) {
    return resume_error(L, "cannot resume non-suspended coroutine");
  }
  if (L->nestedCCallsNum >= LUAI_MAX_C_CALLS) {
    return resume_error(L, "C stack overflow");
  }
  luai_userstateresume(L, nargs);
  assert(L->errFunc == 0);
  L->nestedCCallsBaseNum = ++L->nestedCCallsNum;
  status = luaD_rawrunprotected(L, resume, L->top - nargs);
  if (status != 0) {             /* error? */
    L->status = (uint8_t)status; /* mark thread as `dead' */
    luaD_seterrorobj(L, status, L->top);
    L->ci->top = L->top;
  } else {
    assert(L->nestedCCallsNum == L->nestedCCallsBaseNum);
    status = L->status;
  }
  --L->nestedCCallsNum;
  lua_unlock(L);
  return status;
}

LUA_API int lua_yield(lua_State *L, int nresults) {
  luai_userstateyield(L, nresults);
  lua_lock(L);
  if (L->nestedCCallsNum > L->nestedCCallsBaseNum) {
    luaG_runerror(L, "attempt to yield across metamethod/C-call boundary");
  }
  L->base = L->top - nresults; /* protect stack slots below */
  L->status = LUA_YIELD;
  lua_unlock(L);
  return -1;
}

int luaD_pcall(lua_State *L, Pfunc func, void *u, ptrdiff_t old_top,
               ptrdiff_t ef) {
  int status;
  unsigned short oldnCcalls = L->nestedCCallsNum;
  ptrdiff_t old_ci = saveci(L, L->ci);
  uint8_t old_allowhooks = L->allowHook;
  ptrdiff_t old_errfunc = L->errFunc;
  L->errFunc = ef;
  status = luaD_rawrunprotected(L, func, u);
  if (status != 0) { /* an error occurred? */
    StackIndex oldtop = restorestack(L, old_top);
    luaF_close(L, oldtop); /* close eventual pending closures */
    luaD_seterrorobj(L, status, oldtop);
    L->nestedCCallsNum = oldnCcalls;
    L->ci = restoreci(L, old_ci);
    L->base = L->ci->base;
    L->savedPC = L->ci->savedpc;
    L->allowHook = old_allowhooks;
    restore_stack_limit(L);
  }
  L->errFunc = old_errfunc;
  return status;
}

/*
** Execute a protected parser.
*/
struct SParser {
  ZIO *z;
  Mbuffer buff; /* buffer to be used by the scanner */
  const char *name;
};

static void f_parser(lua_State *L, void *ud) {
  Prototype *tf; // "the function"
  Closure *cl;
  struct SParser *p = ud;
  int c = luaZ_lookahead(p->z);
  luaC_checkGC(L);
  tf = ((c == LUA_SIGNATURE[0]) ? luaU_undump : luaY_parser)(L, p->z, &p->buff,
                                                             p->name);
  cl = luaF_newLclosure(L, tf->upvaluesNum, TABLE_VALUE(GLOBALS(L)));
  cl->l.p = tf;
  for (size_t i = 0; i < tf->upvaluesNum; i++) {
    // Initialize eventual upvalues.
    cl->l.upvalues[i] = luaF_newupval(L);
  }
  SET_CLOSURE(L, L->top, cl);
  incr_top(L);
}

int luaD_protectedparser(lua_State *L, ZIO *z, const char *name) {
  struct SParser p;
  int status;
  p.z = z;
  p.name = name;
  luaZ_initbuffer(L, &p.buff);
  status = luaD_pcall(L, f_parser, &p, savestack(L, L->top), L->errFunc);
  luaZ_freebuffer(L, &p.buff);
  return status;
}
