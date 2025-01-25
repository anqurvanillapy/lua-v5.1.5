/* Stack and Call structure of Lua. */

#include <setjmp.h>

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

// Chain list of long jump buffers.
struct Ctx {
  struct Ctx *prev;
  jmp_buf b;
  volatile int status;
};

void Stack_setErrorObj(lua_State *L, lua_Status errcode, StackIndex oldTop) {
  switch (errcode) {
  case LUA_ERRMEM:
    SET_STRING_TO_STACK(L, oldTop, String_createLiteral(L, MEMERRMSG));
    break;
  case LUA_ERRERR:
    SET_STRING_TO_STACK(L, oldTop,
                        String_createLiteral(L, "error in error handling"));
    break;
  case LUA_ERRSYNTAX:
  case LUA_ERRRUN:
    // Error message on current top.
    SET_OBJECT_TO_SAME_STACK(L, oldTop, L->top - 1);
    break;
  default:
    break;
  }
  L->top = oldTop + 1;
}

static void restoreStackLimit(lua_State *L) {
  assert(L->stackLast - L->stack == L->stackSize - EXTRA_STACK - 1);
  if (L->ciSize > LUAI_MAXCALLS) {
    // There was an overflow.
    ptrdiff_t inUse = L->ci - L->baseCI;
    if (inUse + 1 < LUAI_MAXCALLS) {
      // Can undo overflow.
      Stack_resizeCI(L, LUAI_MAXCALLS);
    }
  }
}

static void resetStack(lua_State *L, lua_Status status) {
  L->ci = L->baseCI;
  L->base = L->ci->base;
  // Close eventual pending closures.
  luaF_close(L, L->base);
  Stack_setErrorObj(L, status, L->base);
  L->nestedCCallsNum = L->nestedCCallsBaseNum;
  L->allowHook = true;
  restoreStackLimit(L);
  L->errFunc = 0;
  L->errorJmp = nullptr;
}

[[noreturn]] void Stack_throw(lua_State *L, lua_Status errcode) {
  if (L->errorJmp) {
    L->errorJmp->status = errcode;
    _longjmp(L->errorJmp->b, 1);
  } else {
    L->status = errcode;
    if (G(L)->panic) {
      resetStack(L, errcode);
      lua_unlock(L);
      G(L)->panic(L);
    }
    exit(EXIT_FAILURE);
  }
}

int Stack_rawrUnprotected(lua_State *L, ProtectedFunc f, void *ud) {
  // Chain new error handler.
  struct Ctx ctx = {.prev = L->errorJmp, .status = 0};
  L->errorJmp = &ctx;
  if (_setjmp(ctx.b) == 0) {
    f(L, ud);
  }
  // Restore old error handler.
  L->errorJmp = ctx.prev;
  return ctx.status;
}

static void correctStack(lua_State *L, Value *oldStack) {
  L->top = (L->top - oldStack) + L->stack;
  for (GCObject *up = L->openUpval; up != NULL; up = up->gch.next) {
    gco2uv(up)->v = (gco2uv(up)->v - oldStack) + L->stack;
  }
  for (CallInfo *ci = L->baseCI; ci <= L->ci; ci++) {
    ci->top = (ci->top - oldStack) + L->stack;
    ci->base = (ci->base - oldStack) + L->stack;
    ci->func = (ci->func - oldStack) + L->stack;
  }
  L->base = (L->base - oldStack) + L->stack;
}

void Stack_resize(lua_State *L, int newSize) {
  Value *oldStack = L->stack;
  int realSize = newSize + 1 + EXTRA_STACK;
  assert(L->stackLast - L->stack == L->stackSize - EXTRA_STACK - 1);
  Mem_reallocVec(L, L->stack, L->stackSize, realSize, Value);
  L->stackSize = realSize;
  L->stackLast = L->stack + newSize;
  correctStack(L, oldStack);
}

void Stack_resizeCI(lua_State *L, int newSize) {
  const CallInfo *oldCI = L->baseCI;
  Mem_reallocVec(L, L->baseCI, L->ciSize, newSize, CallInfo);
  L->ciSize = newSize;
  L->ci += L->baseCI - oldCI;
  L->endCI = L->baseCI + L->ciSize - 1;
}

void Stack_grow(lua_State *L, int n) {
  Stack_resize(L, n <= L->stackSize ? L->stackSize * 2 : L->stackSize + n);
}

static CallInfo *growCI(lua_State *L) {
  if (L->ciSize > LUAI_MAXCALLS) { /* overflow while handling overflow? */
    Stack_throw(L, LUA_ERRERR);
  } else {
    Stack_resizeCI(L, 2 * L->ciSize);
    if (L->ciSize > LUAI_MAXCALLS) {
      luaG_runerror(L, "stack overflow");
    }
  }
  return ++L->ci;
}

void Stack_callHook(lua_State *L, int event, int line) {
  lua_Hook hook = L->hook;
  if (!hook || !L->allowHook) {
    return;
  }
  ptrdiff_t top = SAVE_STACK(L, L->top);
  ptrdiff_t ci_top = SAVE_STACK(L, L->ci->top);
  lua_Debug ar = {
      .event = event,
      .currentline = line,
      // No debug information for tail calls.
      .i_ci = event == LUA_HOOKTAILRET ? 0 : (int)(L->ci - L->baseCI),
  };
  luaD_checkstack(L, LUA_MIN_STACK); /* ensure minimum stack size */
  L->ci->top = L->top + LUA_MIN_STACK;
  assert(L->ci->top <= L->stackLast);
  L->allowHook = false; /* cannot call hooks inside a hook */
  lua_unlock(L);
  hook(L, &ar);
  lua_lock(L);
  assert(!L->allowHook);
  L->allowHook = true;
  L->ci->top = RESTORE_STACK(L, ci_top);
  L->top = RESTORE_STACK(L, top);
}

static StackIndex adjustVarargs(lua_State *L, Prototype *p, int actual) {
  int fixedArgs = p->paramsNum;
  Table *argTable = nullptr;
  while (actual < fixedArgs) {
    SET_NIL(L->top++);
    actual++;
  }

  // Compatible with old-style variadic arguments.
  if (p->varargMode & VARARG_NEEDS_ARG) {
    int extraArgs = actual - fixedArgs;
    assert(p->varargMode & VARARG_HAS_ARG);
    luaC_checkGC(L);
    luaD_checkstack(L, p->maxStackSize);
    argTable = Table_new(L, extraArgs, 1);
    for (int i = 0; i < extraArgs; i++) {
      // Put extra arguments into the `arg` table.
      SET_OBJECT_TO_NEW(L, Table_insertInteger(L, argTable, i + 1),
                        L->top - extraArgs + i);
    }
    // Store counter in field `n`.
    SET_NUMBER(Table_insertString(L, argTable, String_createLiteral(L, "n")),
               (double)extraArgs);
  }

  // Move fixed parameters to the final position.
  StackIndex fixed = L->top - actual; // first fixed argument
  StackIndex base = L->top;           // final position of first argument
  for (int i = 0; i < fixedArgs; i++) {
    SET_OBJECT_TO_SAME_STACK(L, L->top++, fixed + i);
    SET_NIL(fixed + i);
  }
  if (argTable) {
    // Add `arg` parameter.
    SET_TABLE(L, L->top++, argTable);
    assert(iswhite(LuaObjectToGCObject(argTable)));
  }
  return base;
}

static StackIndex tryFuncTM(lua_State *L, StackIndex func) {
  const Value *tm = luaT_gettmbyobj(L, func, TM_CALL);
  ptrdiff_t funcr = SAVE_STACK(L, func);
  if (!IS_TYPE_FUNCTION(tm)) {
    luaG_typeerror(L, func, "call");
  }
  // Open a hole inside the stack at `func`.
  for (StackIndex p = L->top; p > func; p--) {
    SET_OBJECT_TO_SAME_STACK(L, p, p - 1);
  }
  incr_top(L);
  // Previous call may change stack.
  func = RESTORE_STACK(L, funcr);
  // Tag method is the new function to be called.
  SET_OBJECT_TO_STACK(L, func, tm);
  return func;
}

#define INC_CI(L)                                                              \
  (L->ci == L->endCI                                                           \
       ? growCI(L)                                                             \
       : (condhardstacktests(Stack_resizeCI(L, L->ciSize)), ++L->ci))

int Stack_preCall(lua_State *L, StackIndex func, int nresults) {
  if (!IS_TYPE_FUNCTION(func)) {
    func = tryFuncTM(L, func);
  }
  ptrdiff_t funcr = SAVE_STACK(L, func);
  LClosure *cl = &CLOSURE_VALUE(func)->l;
  L->ci->savedpc = L->savedPC;

  if (!cl->header.isC) {
    Prototype *p = cl->p;
    luaD_checkstack(L, p->maxStackSize);
    func = RESTORE_STACK(L, funcr);
    StackIndex base;
    if (!p->varargMode) { /* no varargs? */
      base = func + 1;
      if (L->top > base + p->paramsNum) {
        L->top = base + p->paramsNum;
      }
    } else { /* vararg function */
      int nargs = (int)(L->top - func) - 1;
      base = adjustVarargs(L, p, nargs);
      // Previous call may change the stack.
      func = RESTORE_STACK(L, funcr);
    }
    // Now enter the new function.
    CallInfo *ci = INC_CI(L);
    ci->func = func;
    L->base = base;
    ci->base = base;
    ci->top = L->base + p->maxStackSize;
    assert(ci->top <= L->stackLast);
    L->savedPC = p->code; /* starting point */
    ci->tailcalls = 0;
    ci->nresults = nresults;
    for (StackIndex st = L->top; st < ci->top; st++) {
      SET_NIL(st);
    }
    L->top = ci->top;
    if (L->hookMask & LUA_MASKCALL) {
      // Hooks assume PC is already incremented.
      L->savedPC++;
      Stack_callHook(L, LUA_HOOKCALL, -1);
      // Correct PC.
      L->savedPC--;
    }
    return PCRLUA;
  }

  // It is a C function, just call it.
  luaD_checkstack(L, LUA_MIN_STACK);
  // Now enter the new function.
  CallInfo *ci = INC_CI(L);
  ci->func = RESTORE_STACK(L, funcr);
  L->base = ci->func + 1;
  ci->base = ci->func + 1;
  ci->top = L->top + LUA_MIN_STACK;
  assert(ci->top <= L->stackLast);
  ci->nresults = nresults;
  if (L->hookMask & LUA_MASKCALL) {
    Stack_callHook(L, LUA_HOOKCALL, -1);
  }
  lua_unlock(L);
  int n = CUR_FUNC(L)->c.f(L);
  lua_lock(L);
  if (n < 0) {
    return PCRYIELD;
  }
  luaD_poscall(L, L->top - n);
  return PCRC;
}

static StackIndex callrethooks(lua_State *L, StackIndex firstResult) {
  ptrdiff_t fr = SAVE_STACK(L, firstResult); /* next call may change stack */
  Stack_callHook(L, LUA_HOOKRET, -1);
  if (f_isLua(L->ci)) { /* Lua function? */
    while ((L->hookMask & LUA_MASKRET) && L->ci->tailcalls--) { /* tail calls */
      Stack_callHook(L, LUA_HOOKTAILRET, -1);
    }
  }
  return RESTORE_STACK(L, fr);
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
      Stack_throw(L, LUA_ERRERR); /* error while handing stack error */
    }
  }
  if (Stack_preCall(L, func, nResults) == PCRLUA) { /* is a Lua function? */
    luaV_execute(L, 1);                             /* call it */
  }
  L->nestedCCallsNum--;
  luaC_checkGC(L);
}

static void resume(lua_State *L, void *ud) {
  StackIndex firstArg = cast(StackIndex, ud);
  CallInfo *ci = L->ci;
  if (L->status == LUA_RUNNING) {
    assert(ci == L->baseCI && firstArg > L->base);
    if (Stack_preCall(L, firstArg - 1, LUA_MULTRET) != PCRLUA) {
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
  status = Stack_rawrUnprotected(L, resume, L->top - nargs);
  if (status != 0) {             /* error? */
    L->status = (uint8_t)status; /* mark thread as `dead' */
    Stack_setErrorObj(L, status, L->top);
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

int luaD_pcall(lua_State *L, ProtectedFunc func, void *u, ptrdiff_t old_top,
               ptrdiff_t ef) {
  int status;
  unsigned short oldnCcalls = L->nestedCCallsNum;
  ptrdiff_t old_ci = SAVE_CI(L, L->ci);
  bool allowHook = L->allowHook;
  ptrdiff_t old_errfunc = L->errFunc;
  L->errFunc = ef;
  status = Stack_rawrUnprotected(L, func, u);
  if (status != 0) { /* an error occurred? */
    StackIndex oldtop = RESTORE_STACK(L, old_top);
    luaF_close(L, oldtop); /* close eventual pending closures */
    Stack_setErrorObj(L, status, oldtop);
    L->nestedCCallsNum = oldnCcalls;
    L->ci = RESTORE_CI(L, old_ci);
    L->base = L->ci->base;
    L->savedPC = L->ci->savedpc;
    L->allowHook = allowHook;
    restoreStackLimit(L);
  }
  L->errFunc = old_errfunc;
  return status;
}

/*
** Execute a protected parser.
*/
struct SParser {
  ZIO *z;
  StringBuilder buff; /* buffer to be used by the scanner */
  const char *name;
};

static void f_parser(lua_State *L, void *ud) {
  Prototype *tf; // "the function"
  Closure *cl;
  struct SParser *p = ud;
  int c = luaZ_lookahead(p->z);
  luaC_checkGC(L);
  tf = (c == LUA_SIGNATURE[0] ? luaU_undump : luaY_parser)(L, p->z, &p->buff,
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
  struct SParser p = {.z = z, .name = name};
  StringBuilder_init(L, &p.buff);
  int status = luaD_pcall(L, f_parser, &p, SAVE_STACK(L, L->top), L->errFunc);
  StringBuilder_free(L, &p.buff);
  return status;
}
