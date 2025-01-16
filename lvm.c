/* Lua virtual machine. */

#include <stdio.h>
#include <string.h>

#include "lua.h"

#include "intern.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lopcodes.h"
#include "lstate.h"
#include "ltable.h"
#include "ltm.h"
#include "lvm.h"
#include "object.h"

/* limit for table tag-method chains (to avoid loops) */
#define MAXTAGLOOP 100

const Value *luaV_tonumber(const Value *obj, Value *n) {
  lua_Number num;
  if (IS_TYPE_NUMBER(obj)) {
    return obj;
  }
  if (IS_TYPE_STRING(obj) && luaO_str2d(VALUE_STRING_CONTENT(obj), &num)) {
    SET_NUMBER(n, num);
    return n;
  }
  return nullptr;
}

int luaV_tostring(lua_State *L, StackIndex obj) {
  if (!IS_TYPE_NUMBER(obj)) {
    return 0;
  }
  char s[LUAI_MAXNUMBER2STR];
  lua_Number n = NUMBER_VALUE(obj);
  snprintf(s, sizeof(s), LUA_NUMBER_FMT, n);
  SET_STRING_TO_STACK(L, obj, String_internCStr(L, s));
  return 1;
}

static void traceexec(lua_State *L, const Instruction *pc) {
  uint8_t mask = L->hookMask;
  const Instruction *oldpc = L->savedPC;
  L->savedPC = pc;
  if ((mask & LUA_MASKCOUNT) && L->hookCount == 0) {
    resethookcount(L);
    luaD_callhook(L, LUA_HOOKCOUNT, -1);
  }
  if (mask & LUA_MASKLINE) {
    Prototype *p = ci_func(L->ci)->l.p;
    int npc = pcRel(pc, p);
    int newline = getline(p, npc);
    /* call linehook when enter a new function, when jump back (loop),
       or when enter a new line */
    if (npc == 0 || pc <= oldpc || newline != getline(p, pcRel(oldpc, p))) {
      luaD_callhook(L, LUA_HOOKLINE, newline);
    }
  }
}

static void callTMres(lua_State *L, StackIndex res, const Value *f,
                      const Value *p1, const Value *p2) {
  ptrdiff_t result = savestack(L, res);
  SET_OBJECT_TO_STACK(L, L->top, f);      /* push function */
  SET_OBJECT_TO_STACK(L, L->top + 1, p1); /* 1st argument */
  SET_OBJECT_TO_STACK(L, L->top + 2, p2); /* 2nd argument */
  luaD_checkstack(L, 3);
  L->top += 3;
  luaD_call(L, L->top - 3, 1);
  res = restorestack(L, result);
  L->top--;
  SET_OBJECT_TO_SAME_STACK(L, res, L->top);
}

static void callTM(lua_State *L, const Value *f, const Value *p1,
                   const Value *p2, const Value *p3) {
  SET_OBJECT_TO_STACK(L, L->top, f);      /* push function */
  SET_OBJECT_TO_STACK(L, L->top + 1, p1); /* 1st argument */
  SET_OBJECT_TO_STACK(L, L->top + 2, p2); /* 2nd argument */
  SET_OBJECT_TO_STACK(L, L->top + 3, p3); /* 3th argument */
  luaD_checkstack(L, 4);
  L->top += 4;
  luaD_call(L, L->top - 4, 0);
}

void luaV_gettable(lua_State *L, const Value *t, Value *key, StackIndex val) {
  int loop;
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    const Value *tm;
    if (IS_TYPE_TABLE(t)) { /* `t' is a table? */
      Table *h = TABLE_VALUE(t);
      const Value *res = luaH_get(h, key); /* do a primitive get */
      if (!IS_TYPE_NIL(res) ||             /* result is no nil? */
          (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL) { /* or no TM? */
        SET_OBJECT_TO_STACK(L, val, res);
        return;
      }
      /* else will try the tag method */
    } else if (IS_TYPE_NIL(tm = luaT_gettmbyobj(L, t, TM_INDEX))) {
      luaG_typeerror(L, t, "index");
    }
    if (IS_TYPE_FUNCTION(tm)) {
      callTMres(L, val, tm, t, key);
      return;
    }
    t = tm; /* else repeat with `tm' */
  }
  luaG_runerror(L, "loop in gettable");
}

void luaV_settable(lua_State *L, const Value *t, Value *key, StackIndex val) {
  int loop;
  Value temp;
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    const Value *tm;
    if (IS_TYPE_TABLE(t)) { /* `t' is a table? */
      Table *h = TABLE_VALUE(t);
      Value *oldval = luaH_set(L, h, key); /* do a primitive set */
      if (!IS_TYPE_NIL(oldval) ||          /* result is no nil? */
          (tm = fasttm(L, h->metatable, TM_NEWINDEX)) == NULL) { /* or no TM? */
        SET_OBJECT_TO_TABLE(L, oldval, val);
        h->flags = 0;
        luaC_barriert(L, h, val);
        return;
      }
      /* else will try the tag method */
    } else if (IS_TYPE_NIL(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX))) {
      luaG_typeerror(L, t, "index");
    }
    if (IS_TYPE_FUNCTION(tm)) {
      callTM(L, tm, t, key, val);
      return;
    }
    /* else repeat with `tm' */
    SET_OBJECT(L, &temp, tm); /* avoid pointing inside table (may rehash) */
    t = &temp;
  }
  luaG_runerror(L, "loop in settable");
}

static int call_binTM(lua_State *L, const Value *p1, const Value *p2,
                      StackIndex res, TMS event) {
  const Value *tm = luaT_gettmbyobj(L, p1, event); /* try first operand */
  if (IS_TYPE_NIL(tm)) {
    tm = luaT_gettmbyobj(L, p2, event); /* try second operand */
  }
  if (IS_TYPE_NIL(tm)) {
    return 0;
  }
  callTMres(L, res, tm, p1, p2);
  return 1;
}

static const Value *get_compTM(lua_State *L, Table *mt1, Table *mt2,
                               TMS event) {
  const Value *tm1 = fasttm(L, mt1, event);
  const Value *tm2;
  if (tm1 == NULL) {
    return NULL; /* no metamethod */
  }
  if (mt1 == mt2) {
    return tm1; /* same metatables => same metamethods */
  }
  tm2 = fasttm(L, mt2, event);
  if (tm2 == NULL) {
    return NULL; /* no metamethod */
  }
  if (luaO_rawequalObj(tm1, tm2)) { /* same metamethods? */
    return tm1;
  }
  return NULL;
}

static int call_orderTM(lua_State *L, const Value *p1, const Value *p2,
                        TMS event) {
  const Value *tm1 = luaT_gettmbyobj(L, p1, event);
  const Value *tm2;
  if (IS_TYPE_NIL(tm1)) {
    return -1; /* no metamethod? */
  }
  tm2 = luaT_gettmbyobj(L, p2, event);
  if (!luaO_rawequalObj(tm1, tm2)) { /* different metamethods? */
    return -1;
  }
  callTMres(L, L->top, tm1, p1, p2);
  return !IS_FALSE(L->top);
}

static int l_strcmp(const StringHeader *ls, const StringHeader *rs) {
  const char *l = STRING_CONTENT(ls);
  size_t ll = ls->len;
  const char *r = STRING_CONTENT(rs);
  size_t lr = rs->len;
  for (;;) {
    int temp = strcoll(l, r);
    if (temp != 0) {
      return temp;
    } else {                  /* strings are equal up to a `\0' */
      size_t len = strlen(l); /* index of first `\0' in both strings */
      if (len == lr) {        /* r is finished? */
        return (len == ll) ? 0 : 1;
      } else if (len == ll) { /* l is finished? */
        return -1; /* l is smaller than r (because r is not finished) */
      }
      /* both strings longer than `len'; go on comparing (after the `\0') */
      len++;
      l += len;
      ll -= len;
      r += len;
      lr -= len;
    }
  }
}

int luaV_lessthan(lua_State *L, const Value *l, const Value *r) {
  int res;
  if (GET_TYPE(l) != GET_TYPE(r)) {
    return luaG_ordererror(L, l, r);
  } else if (IS_TYPE_NUMBER(l)) {
    return luai_numlt(NUMBER_VALUE(l), NUMBER_VALUE(r));
  } else if (IS_TYPE_STRING(l)) {
    return l_strcmp(STRING_VALUE(l), STRING_VALUE(r)) < 0;
  } else if ((res = call_orderTM(L, l, r, TM_LT)) != -1) {
    return res;
  }
  return luaG_ordererror(L, l, r);
}

static int lessequal(lua_State *L, const Value *l, const Value *r) {
  int res;
  if (GET_TYPE(l) != GET_TYPE(r)) {
    return luaG_ordererror(L, l, r);
  } else if (IS_TYPE_NUMBER(l)) {
    return luai_numle(NUMBER_VALUE(l), NUMBER_VALUE(r));
  } else if (IS_TYPE_STRING(l)) {
    return l_strcmp(STRING_VALUE(l), STRING_VALUE(r)) <= 0;
  } else if ((res = call_orderTM(L, l, r, TM_LE)) != -1) { /* first try `le' */
    return res;
  } else if ((res = call_orderTM(L, r, l, TM_LT)) != -1) { /* else try `lt' */
    return !res;
  }
  return luaG_ordererror(L, l, r);
}

int luaV_equalval(lua_State *L, const Value *t1, const Value *t2) {
  const Value *tm;
  DEBUG_ASSERT(GET_TYPE(t1) == GET_TYPE(t2));
  switch (GET_TYPE(t1)) {
  case LUA_TYPE_NIL:
    return 1;
  case LUA_TYPE_NUMBER:
    return luai_numeq(NUMBER_VALUE(t1), NUMBER_VALUE(t2));
  case LUA_TYPE_BOOLEAN:
    return BOOL_VALUE(t1) == BOOL_VALUE(t2); /* true must be 1 !! */
  case LUA_TYPE_PTR:
    return PTR_VALUE(t1) == PTR_VALUE(t2);
  case LUA_TYPE_USERDATA: {
    if (USERDATA_VALUE(t1) == USERDATA_VALUE(t2)) {
      return 1;
    }
    tm = get_compTM(L, USERDATA_VALUE(t1)->metatable,
                    USERDATA_VALUE(t2)->metatable, TM_EQ);
    break; /* will try TM */
  }
  case LUA_TYPE_TABLE: {
    if (TABLE_VALUE(t1) == TABLE_VALUE(t2)) {
      return 1;
    }
    tm = get_compTM(L, TABLE_VALUE(t1)->metatable, TABLE_VALUE(t2)->metatable,
                    TM_EQ);
    break; /* will try TM */
  }
  default:
    return GC_VALUE(t1) == GC_VALUE(t2);
  }
  if (tm == NULL) {
    return 0; /* no TM? */
  }
  callTMres(L, L->top, tm, t1, t2); /* call TM */
  return !IS_FALSE(L->top);
}

void luaV_concat(lua_State *L, int total, int last) {
  do {
    StackIndex top = L->base + last + 1;
    int n = 2; /* number of elements handled in this pass (at least 2) */
    if (!(IS_TYPE_STRING(top - 2) || IS_TYPE_NUMBER(top - 2)) ||
        !tostring(L, top - 1)) {
      if (!call_binTM(L, top - 2, top - 1, top - 2, TM_CONCAT)) {
        luaG_concaterror(L, top - 2, top - 1);
      }
    } else if (STRING_VALUE(top - 1)->len == 0) { /* second op is empty? */
      (void)tostring(L, top - 2); /* result is first op (as string) */
    } else {
      /* at least two string values; get as many as possible */
      size_t tl = STRING_VALUE(top - 1)->len;
      char *buffer;
      int i;
      /* collect total length */
      for (n = 1; n < total && tostring(L, top - n - 1); n++) {
        size_t l = STRING_VALUE(top - n - 1)->len;
        if (l >= MAX_SIZET - tl) {
          luaG_runerror(L, "string length overflow");
        }
        tl += l;
      }
      buffer = luaZ_openspace(L, &G(L)->buff, tl);
      tl = 0;
      for (i = n; i > 0; i--) { /* concat all strings */
        size_t l = STRING_VALUE(top - i)->len;
        memcpy(buffer + tl, VALUE_STRING_CONTENT(top - i), l);
        tl += l;
      }
      SET_STRING_TO_STACK(L, top - n, String_intern(L, buffer, tl));
    }
    total -= n - 1; /* got `n' strings to create 1 new */
    last -= n - 1;
  } while (total > 1); /* repeat until only 1 result left */
}

static void Arith(lua_State *L, StackIndex ra, const Value *rb, const Value *rc,
                  TMS op) {
  Value tempb, tempc;
  const Value *b, *c;
  if ((b = luaV_tonumber(rb, &tempb)) != NULL &&
      (c = luaV_tonumber(rc, &tempc)) != NULL) {
    lua_Number nb = NUMBER_VALUE(b), nc = NUMBER_VALUE(c);
    switch (op) {
    case TM_ADD:
      SET_NUMBER(ra, luai_numadd(nb, nc));
      break;
    case TM_SUB:
      SET_NUMBER(ra, luai_numsub(nb, nc));
      break;
    case TM_MUL:
      SET_NUMBER(ra, luai_nummul(nb, nc));
      break;
    case TM_DIV:
      SET_NUMBER(ra, luai_numdiv(nb, nc));
      break;
    case TM_MOD:
      SET_NUMBER(ra, luai_nummod(nb, nc));
      break;
    case TM_POW:
      SET_NUMBER(ra, luai_numpow(nb, nc));
      break;
    case TM_UNM:
      SET_NUMBER(ra, luai_numunm(nb));
      break;
    default:
      DEBUG_ASSERT(0);
      break;
    }
  } else if (!call_binTM(L, rb, rc, ra, op)) {
    luaG_aritherror(L, rb, rc);
  }
}

/*
** some macros for common tasks in `luaV_execute'
*/

#define runtime_check(L, c)                                                    \
  {                                                                            \
    if (!(c))                                                                  \
      break;                                                                   \
  }

#define RA(i) (base + GETARG_A(i))
/* to be used after possible stack reallocation */
#define RB(i) CHECK_EXPR(getBMode(GET_OPCODE(i)) == OpArgR, base + GETARG_B(i))
#define RC(i) CHECK_EXPR(getCMode(GET_OPCODE(i)) == OpArgR, base + GETARG_C(i))
#define RKB(i)                                                                 \
  CHECK_EXPR(getBMode(GET_OPCODE(i)) == OpArgK,                                \
             ISK(GETARG_B(i)) ? k + INDEXK(GETARG_B(i)) : base + GETARG_B(i))
#define RKC(i)                                                                 \
  CHECK_EXPR(getCMode(GET_OPCODE(i)) == OpArgK,                                \
             ISK(GETARG_C(i)) ? k + INDEXK(GETARG_C(i)) : base + GETARG_C(i))
#define KBx(i) CHECK_EXPR(getBMode(GET_OPCODE(i)) == OpArgK, k + GETARG_Bx(i))

#define dojump(L, pc, i)                                                       \
  {                                                                            \
    (pc) += (i);                                                               \
    luai_threadyield(L);                                                       \
  }

#define Protect(x)                                                             \
  {                                                                            \
    L->savedPC = pc;                                                           \
    {                                                                          \
      x;                                                                       \
    };                                                                         \
    base = L->base;                                                            \
  }

#define arith_op(op, tm)                                                       \
  {                                                                            \
    Value *rb = RKB(i);                                                        \
    Value *rc = RKC(i);                                                        \
    if (IS_TYPE_NUMBER(rb) && IS_TYPE_NUMBER(rc)) {                            \
      lua_Number nb = NUMBER_VALUE(rb), nc = NUMBER_VALUE(rc);                 \
      SET_NUMBER(ra, op(nb, nc));                                              \
    } else                                                                     \
      Protect(Arith(L, ra, rb, rc, tm));                                       \
  }

void luaV_execute(lua_State *L, int nexeccalls) {
  LClosure *cl;
  StackIndex base;
  Value *k;
  const Instruction *pc;
reentry: /* entry point */
  DEBUG_ASSERT(isLua(L->ci));
  pc = L->savedPC;
  cl = &CLOSURE_VALUE(L->ci->func)->l;
  base = L->base;
  k = cl->p->k;
  /* main loop of interpreter */
  for (;;) {
    const Instruction i = *pc++;
    StackIndex ra;
    if ((L->hookMask & (LUA_MASKLINE | LUA_MASKCOUNT)) &&
        (--L->hookCount == 0 || L->hookMask & LUA_MASKLINE)) {
      traceexec(L, pc);
      if (L->status == LUA_YIELD) { /* did hook yield? */
        L->savedPC = pc - 1;
        return;
      }
      base = L->base;
    }
    /* warning!! several calls may realloc the stack and invalidate `ra' */
    ra = RA(i);
    DEBUG_ASSERT(base == L->base && L->base == L->ci->base);
    DEBUG_ASSERT(base <= L->top && L->top <= L->stack + L->stackSize);
    DEBUG_ASSERT(L->top == L->ci->top || luaG_checkopenop(i));
    switch (GET_OPCODE(i)) {
    case OP_MOVE: {
      SET_OBJECT_TO_SAME_STACK(L, ra, RB(i));
      continue;
    }
    case OP_LOADK: {
      SET_OBJECT_TO_STACK(L, ra, KBx(i));
      continue;
    }
    case OP_LOADBOOL: {
      SET_BOOL(ra, GETARG_B(i));
      if (GETARG_C(i)) {
        pc++; /* skip next instruction (if C) */
      }
      continue;
    }
    case OP_LOADNIL: {
      Value *rb = RB(i);
      do {
        SET_NIL(rb--);
      } while (rb >= ra);
      continue;
    }
    case OP_GETUPVAL: {
      int b = GETARG_B(i);
      SET_OBJECT_TO_STACK(L, ra, cl->upvalues[b]->v);
      continue;
    }
    case OP_GETGLOBAL: {
      Value g;
      Value *rb = KBx(i);
      SET_TABLE(L, &g, cl->header.env);
      DEBUG_ASSERT(IS_TYPE_STRING(rb));
      Protect(luaV_gettable(L, &g, rb, ra));
      continue;
    }
    case OP_GETTABLE: {
      Protect(luaV_gettable(L, RB(i), RKC(i), ra));
      continue;
    }
    case OP_SETGLOBAL: {
      Value g;
      SET_TABLE(L, &g, cl->header.env);
      DEBUG_ASSERT(IS_TYPE_STRING(KBx(i)));
      Protect(luaV_settable(L, &g, KBx(i), ra));
      continue;
    }
    case OP_SETUPVAL: {
      Upvalue *uv = cl->upvalues[GETARG_B(i)];
      SET_OBJECT(L, uv->v, ra);
      luaC_barrier(L, uv, ra);
      continue;
    }
    case OP_SETTABLE: {
      Protect(luaV_settable(L, ra, RKB(i), RKC(i)));
      continue;
    }
    case OP_NEWTABLE: {
      int b = GETARG_B(i);
      int c = GETARG_C(i);
      SET_TABLE(L, ra, luaH_new(L, luaO_fb2int(b), luaO_fb2int(c)));
      Protect(luaC_checkGC(L));
      continue;
    }
    case OP_SELF: {
      StackIndex rb = RB(i);
      SET_OBJECT_TO_SAME_STACK(L, ra + 1, rb);
      Protect(luaV_gettable(L, rb, RKC(i), ra));
      continue;
    }
    case OP_ADD: {
      arith_op(luai_numadd, TM_ADD);
      continue;
    }
    case OP_SUB: {
      arith_op(luai_numsub, TM_SUB);
      continue;
    }
    case OP_MUL: {
      arith_op(luai_nummul, TM_MUL);
      continue;
    }
    case OP_DIV: {
      arith_op(luai_numdiv, TM_DIV);
      continue;
    }
    case OP_MOD: {
      arith_op(luai_nummod, TM_MOD);
      continue;
    }
    case OP_POW: {
      arith_op(luai_numpow, TM_POW);
      continue;
    }
    case OP_UNM: {
      Value *rb = RB(i);
      if (IS_TYPE_NUMBER(rb)) {
        lua_Number nb = NUMBER_VALUE(rb);
        SET_NUMBER(ra, luai_numunm(nb));
      } else {
        Protect(Arith(L, ra, rb, rb, TM_UNM));
      }
      continue;
    }
    case OP_NOT: {
      int res = IS_FALSE(RB(i)); /* next assignment may change this value */
      SET_BOOL(ra, res);
      continue;
    }
    case OP_LEN: {
      const Value *rb = RB(i);
      switch (GET_TYPE(rb)) {
      case LUA_TYPE_TABLE: {
        SET_NUMBER(ra, cast_num(luaH_getn(TABLE_VALUE(rb))));
        break;
      }
      case LUA_TYPE_STRING: {
        SET_NUMBER(ra, cast_num(STRING_VALUE(rb)->len));
        break;
      }
      default: { /* try metamethod */
        Protect(if (!call_binTM(L, rb, luaO_nilobject, ra, TM_LEN))
                    luaG_typeerror(L, rb, "get length of");)
      }
      }
      continue;
    }
    case OP_CONCAT: {
      int b = GETARG_B(i);
      int c = GETARG_C(i);
      Protect(luaV_concat(L, c - b + 1, c); luaC_checkGC(L));
      SET_OBJECT_TO_SAME_STACK(L, RA(i), base + b);
      continue;
    }
    case OP_JMP: {
      dojump(L, pc, GETARG_sBx(i));
      continue;
    }
    case OP_EQ: {
      Value *rb = RKB(i);
      Value *rc = RKC(i);
      Protect(if (equalobj(L, rb, rc) == GETARG_A(i))
                  dojump(L, pc, GETARG_sBx(*pc));) pc++;
      continue;
    }
    case OP_LT: {
      Protect(if (luaV_lessthan(L, RKB(i), RKC(i)) == GETARG_A(i))
                  dojump(L, pc, GETARG_sBx(*pc));) pc++;
      continue;
    }
    case OP_LE: {
      Protect(if (lessequal(L, RKB(i), RKC(i)) == GETARG_A(i))
                  dojump(L, pc, GETARG_sBx(*pc));) pc++;
      continue;
    }
    case OP_TEST: {
      if (IS_FALSE(ra) != GETARG_C(i))
        dojump(L, pc, GETARG_sBx(*pc));
      pc++;
      continue;
    }
    case OP_TESTSET: {
      Value *rb = RB(i);
      if (IS_FALSE(rb) != GETARG_C(i)) {
        SET_OBJECT_TO_SAME_STACK(L, ra, rb);
        dojump(L, pc, GETARG_sBx(*pc));
      }
      pc++;
      continue;
    }
    case OP_CALL: {
      int b = GETARG_B(i);
      int nresults = GETARG_C(i) - 1;
      if (b != 0) {
        L->top = ra + b; /* else previous instruction set top */
      }
      L->savedPC = pc;
      switch (luaD_precall(L, ra, nresults)) {
      case PCRLUA: {
        nexeccalls++;
        goto reentry; /* restart luaV_execute over new Lua function */
      }
      case PCRC: {
        /* it was a C function (`precall' called it); adjust results */
        if (nresults >= 0) {
          L->top = L->ci->top;
        }
        base = L->base;
        continue;
      }
      default: {
        return; /* yield */
      }
      }
    }
    case OP_TAILCALL: {
      int b = GETARG_B(i);
      if (b != 0) {
        L->top = ra + b; /* else previous instruction set top */
      }
      L->savedPC = pc;
      DEBUG_ASSERT(GETARG_C(i) - 1 == LUA_MULTRET);
      switch (luaD_precall(L, ra, LUA_MULTRET)) {
      case PCRLUA: {
        /* tail call: put new frame in place of previous one */
        CallInfo *ci = L->ci - 1; /* previous frame */
        int aux;
        StackIndex func = ci->func;
        StackIndex pfunc = (ci + 1)->func; /* previous function index */
        if (L->openUpval) {
          luaF_close(L, ci->base);
        }
        L->base = ci->base = ci->func + ((ci + 1)->base - pfunc);
        for (aux = 0; pfunc + aux < L->top; aux++) /* move frame down */
          SET_OBJECT_TO_SAME_STACK(L, func + aux, pfunc + aux);
        ci->top = L->top = func + aux; /* correct top */
        DEBUG_ASSERT(L->top ==
                     L->base + CLOSURE_VALUE(func)->l.p->maxStackSize);
        ci->savedpc = L->savedPC;
        ci->tailcalls++; /* one more call lost */
        L->ci--;         /* remove new frame */
        goto reentry;
      }
      case PCRC: { /* it was a C function (`precall' called it) */
        base = L->base;
        continue;
      }
      default: {
        return; /* yield */
      }
      }
    }
    case OP_RETURN: {
      int b = GETARG_B(i);
      if (b != 0) {
        L->top = ra + b - 1;
      }
      if (L->openUpval) {
        luaF_close(L, base);
      }
      L->savedPC = pc;
      b = luaD_poscall(L, ra);
      if (--nexeccalls == 0) { /* was previous function running `here'? */
        return;                /* no: return */
      } else {                 /* yes: continue its execution */
        if (b) {
          L->top = L->ci->top;
        }
        DEBUG_ASSERT(isLua(L->ci));
        DEBUG_ASSERT(GET_OPCODE(*((L->ci)->savedpc - 1)) == OP_CALL);
        goto reentry;
      }
    }
    case OP_FORLOOP: {
      lua_Number step = NUMBER_VALUE(ra + 2);
      lua_Number idx =
          luai_numadd(NUMBER_VALUE(ra), step); /* increment index */
      lua_Number limit = NUMBER_VALUE(ra + 1);
      if (luai_numlt(0, step) ? luai_numle(idx, limit)
                              : luai_numle(limit, idx)) {
        dojump(L, pc, GETARG_sBx(i)); /* jump back */
        SET_NUMBER(ra, idx);          /* update internal index... */
        SET_NUMBER(ra + 3, idx);      /* ...and external index */
      }
      continue;
    }
    case OP_FORPREP: {
      const Value *init = ra;
      const Value *plimit = ra + 1;
      const Value *pstep = ra + 2;
      L->savedPC = pc; /* next steps may throw errors */
      if (!tonumber(init, ra)) {
        luaG_runerror(L, LUA_QL("for") " initial value must be a number");
      } else if (!tonumber(plimit, ra + 1)) {
        luaG_runerror(L, LUA_QL("for") " limit must be a number");
      } else if (!tonumber(pstep, ra + 2)) {
        luaG_runerror(L, LUA_QL("for") " step must be a number");
      }
      SET_NUMBER(ra, luai_numsub(NUMBER_VALUE(ra), NUMBER_VALUE(pstep)));
      dojump(L, pc, GETARG_sBx(i));
      continue;
    }
    case OP_TFORLOOP: {
      StackIndex cb = ra + 3; /* call base */
      SET_OBJECT_TO_SAME_STACK(L, cb + 2, ra + 2);
      SET_OBJECT_TO_SAME_STACK(L, cb + 1, ra + 1);
      SET_OBJECT_TO_SAME_STACK(L, cb, ra);
      L->top = cb + 3; /* func. + 2 args (state and index) */
      Protect(luaD_call(L, cb, GETARG_C(i)));
      L->top = L->ci->top;
      cb = RA(i) + 3;         /* previous call may change the stack */
      if (!IS_TYPE_NIL(cb)) { /* continue loop? */
        SET_OBJECT_TO_SAME_STACK(L, cb - 1, cb); /* save control variable */
        dojump(L, pc, GETARG_sBx(*pc));          /* jump back */
      }
      pc++;
      continue;
    }
    case OP_SETLIST: {
      int n = GETARG_B(i);
      int c = GETARG_C(i);
      int last;
      Table *h;
      if (n == 0) {
        n = cast_int(L->top - ra) - 1;
        L->top = L->ci->top;
      }
      if (c == 0) {
        c = cast_int(*pc++);
      }
      runtime_check(L, IS_TYPE_TABLE(ra));
      h = TABLE_VALUE(ra);
      last = ((c - 1) * LFIELDS_PER_FLUSH) + n;
      if (last > h->sizearray) {      /* needs more space? */
        luaH_resizearray(L, h, last); /* pre-alloc it at once */
      }
      for (; n > 0; n--) {
        Value *val = ra + n;
        SET_OBJECT_TO_TABLE(L, luaH_setnum(L, h, last--), val);
        luaC_barriert(L, h, val);
      }
      continue;
    }
    case OP_CLOSE: {
      luaF_close(L, ra);
      continue;
    }
    case OP_CLOSURE: {
      Prototype *p;
      Closure *ncl;
      int nup, j;
      p = cl->p->inners[GETARG_Bx(i)];
      nup = p->upvaluesNum;
      ncl = luaF_newLclosure(L, nup, cl->header.env);
      ncl->l.p = p;
      for (j = 0; j < nup; j++, pc++) {
        if (GET_OPCODE(*pc) == OP_GETUPVAL) {
          ncl->l.upvalues[j] = cl->upvalues[GETARG_B(*pc)];
        } else {
          DEBUG_ASSERT(GET_OPCODE(*pc) == OP_MOVE);
          ncl->l.upvalues[j] = luaF_findupval(L, base + GETARG_B(*pc));
        }
      }
      SET_CLOSURE(L, ra, ncl);
      Protect(luaC_checkGC(L));
      continue;
    }
    case OP_VARARG: {
      int b = GETARG_B(i) - 1;
      int j;
      CallInfo *ci = L->ci;
      int n = cast_int(ci->base - ci->func) - cl->p->paramsNum - 1;
      if (b == LUA_MULTRET) {
        Protect(luaD_checkstack(L, n));
        ra = RA(i); /* previous call may change the stack */
        b = n;
        L->top = ra + n;
      }
      for (j = 0; j < b; j++) {
        if (j < n) {
          SET_OBJECT_TO_SAME_STACK(L, ra + j, ci->base - n + j);
        } else {
          SET_NIL(ra + j);
        }
      }
      continue;
    }
    }
  }
}
