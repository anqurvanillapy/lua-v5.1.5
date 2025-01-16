/* Garbage Collector. */

#include <string.h>

#include "lua.h"

#include "intern.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lstate.h"
#include "ltable.h"
#include "ltm.h"
#include "object.h"

#define GCSTEPSIZE 1024u
#define GCSWEEPMAX 40
#define GCSWEEPCOST 10
#define GCFINALIZECOST 100

#define maskmarks cast_byte(~(bitmask(BLACKBIT) | WHITEBITS))

#define makewhite(g, x)                                                        \
  ((x)->gch.marked = cast_byte(((x)->gch.marked & maskmarks) | luaC_white(g)))

#define white2gray(x) reset2bits((x)->gch.marked, WHITE0BIT, WHITE1BIT)
#define black2gray(x) resetbit((x)->gch.marked, BLACKBIT)

#define stringmark(s) reset2bits((s)->tsv.header.marked, WHITE0BIT, WHITE1BIT)

#define isfinalized(u) testbit((u)->header.marked, FINALIZEDBIT)
#define markfinalized(u) l_setbit((u)->header.marked, FINALIZEDBIT)

#define KEYWEAK bitmask(KEYWEAKBIT)
#define VALUEWEAK bitmask(VALUEWEAKBIT)

#define markvalue(g, o)                                                        \
  do {                                                                         \
    DEBUG_CHECK_CONSISTENCY(o);                                                \
    if (IS_COLLECTABLE(o) && iswhite(GC_VALUE(o))) {                           \
      reallymarkobject(g, GC_VALUE(o));                                        \
    }                                                                          \
  } while (false)

#define markobject(g, t)                                                       \
  do {                                                                         \
    if (iswhite(LuaObjectToGCObject(t)))                                       \
      reallymarkobject(g, LuaObjectToGCObject(t));                             \
  } while (false)

#define setthreshold(g) (g->GCthreshold = (g->estimate / 100) * g->gcpause)

static void removeentry(Node *n) {
  DEBUG_ASSERT(IS_TYPE_NIL(gval(n)));
  if (IS_COLLECTABLE(gkey(n))) {
    SET_TYPE(gkey(n), LUA_TYPE_DEAD); /* dead key; remove it */
  }
}

static void reallymarkobject(global_State *g, GCObject *o) {
  DEBUG_ASSERT(iswhite(o) && !IS_DEAD(g, o));
  white2gray(o);
  switch (o->gch.tt) {
  case LUA_TYPE_STRING: {
    return;
  }
  case LUA_TYPE_USERDATA: {
    Table *mt = gco2u(o)->metatable;
    gray2black(o); /* udata are never gray */
    if (mt)
      markobject(g, mt);
    markobject(g, gco2u(o)->env);
    return;
  }
  case LUA_TYPE_UPVALUE: {
    Upvalue *uv = gco2uv(o);
    markvalue(g, uv->v);
    if (uv->v == &uv->u.value) { /* closed? */
      gray2black(o);             /* open upvalues are never black */
    }
    return;
  }
  case LUA_TYPE_FUNCTION: {
    gco2cl(o)->c.header.gclist = g->gray;
    g->gray = o;
    break;
  }
  case LUA_TYPE_TABLE: {
    gco2h(o)->gclist = g->gray;
    g->gray = o;
    break;
  }
  case LUA_TYPE_THREAD: {
    gco2th(o)->gcList = g->gray;
    g->gray = o;
    break;
  }
  case LUA_TYPE_PROTO: {
    gco2p(o)->gcList = g->gray;
    g->gray = o;
    break;
  }
  default:
    DEBUG_ASSERT(0);
  }
}

static void marktmu(global_State *g) {
  GCObject *u = g->tmudata;
  if (u) {
    do {
      u = u->gch.next;
      makewhite(g, u); /* may be marked, if left from previous GC */
      reallymarkobject(g, u);
    } while (u != g->tmudata);
  }
}

/* move `dead' udata that need finalization to list `tmudata' */
size_t luaC_separateudata(lua_State *L, int all) {
  global_State *g = G(L);
  size_t deadmem = 0;
  GCObject **p = &g->mainthread->header.next;
  GCObject *curr;
  while ((curr = *p) != NULL) {
    if (!(iswhite(curr) || all) || isfinalized(gco2u(curr))) {
      p = &curr->gch.next; /* don't bother with them */
    } else if (fasttm(L, gco2u(curr)->metatable, TM_GC) == NULL) {
      markfinalized(gco2u(curr)); /* don't need finalization */
      p = &curr->gch.next;
    } else { /* must call its gc method */
      deadmem += sizeudata(gco2u(curr));
      markfinalized(gco2u(curr));
      *p = curr->gch.next;
      /* link `curr' at the end of `tmudata' list */
      if (g->tmudata == NULL) {             /* list is empty? */
        g->tmudata = curr->gch.next = curr; /* creates a circular list */
      } else {
        curr->gch.next = g->tmudata->gch.next;
        g->tmudata->gch.next = curr;
        g->tmudata = curr;
      }
    }
  }
  return deadmem;
}

static int traversetable(global_State *g, Table *h) {
  int i;
  int weakkey = 0;
  int weakvalue = 0;
  const Value *mode;
  if (h->metatable)
    markobject(g, h->metatable);
  mode = gfasttm(g, h->metatable, TM_MODE);
  if (mode && IS_TYPE_STRING(mode)) { /* is there a weak mode? */
    weakkey = (strchr(GET_STR_VALUE(mode), 'k') != NULL);
    weakvalue = (strchr(GET_STR_VALUE(mode), 'v') != NULL);
    if (weakkey || weakvalue) {                   /* is really weak? */
      h->header.marked &= ~(KEYWEAK | VALUEWEAK); /* clear bits */
      h->header.marked |=
          cast_byte((weakkey << KEYWEAKBIT) | (weakvalue << VALUEWEAKBIT));
      h->gclist = g->weak;              /* must be cleared after GC, ... */
      g->weak = LuaObjectToGCObject(h); /* ... so put in the appropriate list */
    }
  }
  if (weakkey && weakvalue) {
    return 1;
  }
  if (!weakvalue) {
    i = h->sizearray;
    while (i--) {
      markvalue(g, &h->array[i]);
    }
  }
  i = sizenode(h);
  while (i--) {
    Node *n = gnode(h, i);
    DEBUG_ASSERT(GET_TYPE(gkey(n)) != LUA_TYPE_DEAD || IS_TYPE_NIL(gval(n)));
    if (IS_TYPE_NIL(gval(n))) {
      removeentry(n); /* remove empty entries */
    } else {
      DEBUG_ASSERT(!IS_TYPE_NIL(gkey(n)));
      if (!weakkey)
        markvalue(g, gkey(n));
      if (!weakvalue)
        markvalue(g, gval(n));
    }
  }
  return weakkey || weakvalue;
}

/*
** All marks are conditional because a GC may happen while the
** prototype is still being created
*/
static void traverseproto(global_State *g, Prototype *f) {
  int i;
  if (f->source) {
    stringmark(f->source);
  }
  for (i = 0; i < f->kSize; i++) /* mark literals */
    markvalue(g, &f->k[i]);
  for (i = 0; i < f->upvaluesSize; i++) { /* mark upvalue names */
    if (f->upvalues[i]) {
      stringmark(f->upvalues[i]);
    }
  }
  for (i = 0; i < f->pSize; i++) { /* mark nested protos */
    if (f->inners[i])
      markobject(g, f->inners[i]);
  }
  for (i = 0; i < f->locVarsSize; i++) { /* mark local-variable names */
    if (f->locVars[i].varname) {
      stringmark(f->locVars[i].varname);
    }
  }
}

static void traverseclosure(global_State *g, Closure *cl) {
  markobject(g, cl->c.header.env);
  if (cl->c.header.isC) {
    int i;
    for (i = 0; i < cl->c.header.nupvalues; i++) /* mark its upvalues */
      markvalue(g, &cl->c.upvalue[i]);
  } else {
    int i;
    DEBUG_ASSERT(cl->l.header.nupvalues == cl->l.p->upvaluesNum);
    markobject(g, cl->l.p);
    for (i = 0; i < cl->l.header.nupvalues; i++) /* mark its upvalues */
      markobject(g, cl->l.upvalues[i]);
  }
}

static void checkstacksizes(lua_State *L, StackIndex max) {
  int ci_used = cast_int(L->ci - L->baseCI); /* number of `ci' in use */
  int s_used = cast_int(max - L->stack);     /* part of stack in use */
  if (L->ciSize > LUAI_MAXCALLS) {           /* handling overflow? */
    return;                                  /* do not touch the stacks */
  }
  if (4 * ci_used < L->ciSize && 2 * BASIC_CI_SIZE < L->ciSize) {
    luaD_reallocCI(L, L->ciSize / 2); /* still big enough... */
  }
  condhardstacktests(luaD_reallocCI(L, ci_used + 1));
  if (4 * s_used < L->stackSize &&
      2 * (BASIC_STACK_SIZE + EXTRA_STACK) < L->stackSize) {
    luaD_reallocstack(L, L->stackSize / 2); /* still big enough... */
  }
  condhardstacktests(luaD_reallocstack(L, s_used));
}

static void traversestack(global_State *g, lua_State *l) {
  StackIndex o, lim;
  CallInfo *ci;
  markvalue(g, gt(l));
  lim = l->top;
  for (ci = l->baseCI; ci <= l->ci; ci++) {
    DEBUG_ASSERT(ci->top <= l->stackLast);
    if (lim < ci->top) {
      lim = ci->top;
    }
  }
  for (o = l->stack; o < l->top; o++)
    markvalue(g, o);
  for (; o <= lim; o++) {
    SET_NIL(o);
  }
  checkstacksizes(l, lim);
}

/*
** traverse one gray object, turning it to black.
** Returns `quantity' traversed.
*/
static l_mem propagatemark(global_State *g) {
  GCObject *o = g->gray;
  DEBUG_ASSERT(isgray(o));
  gray2black(o);
  switch (o->gch.tt) {
  case LUA_TYPE_TABLE: {
    Table *h = gco2h(o);
    g->gray = h->gclist;
    if (traversetable(g, h)) { /* table is weak? */
      black2gray(o);           /* keep it gray */
    }
    return sizeof(Table) + sizeof(Value) * h->sizearray +
           sizeof(Node) * sizenode(h);
  }
  case LUA_TYPE_FUNCTION: {
    Closure *cl = gco2cl(o);
    g->gray = cl->c.header.gclist;
    traverseclosure(g, cl);
    return (cl->c.header.isC) ? sizeCclosure(cl->c.header.nupvalues)
                              : sizeLclosure(cl->l.header.nupvalues);
  }
  case LUA_TYPE_THREAD: {
    lua_State *th = gco2th(o);
    g->gray = th->gcList;
    th->gcList = g->grayagain;
    g->grayagain = o;
    black2gray(o);
    traversestack(g, th);
    return sizeof(lua_State) + sizeof(Value) * th->stackSize +
           sizeof(CallInfo) * th->ciSize;
  }
  case LUA_TYPE_PROTO: {
    Prototype *p = gco2p(o);
    g->gray = p->gcList;
    traverseproto(g, p);
    return sizeof(Prototype) + sizeof(Instruction) * p->codeSize +
           sizeof(Prototype *) * p->pSize + sizeof(Value) * p->kSize +
           sizeof(int) * p->lineInfoSize + sizeof(LocVar) * p->locVarsSize +
           sizeof(TString *) * p->upvaluesSize;
  }
  default:
    DEBUG_ASSERT(0);
    return 0;
  }
}

static size_t propagateall(global_State *g) {
  size_t m = 0;
  while (g->gray) {
    m += propagatemark(g);
  }
  return m;
}

/*
** The next function tells whether a key or value can be cleared from
** a weak table. Non-collectable objects are never removed from weak
** tables. Strings behave as `values', so are never removed too. for
** other objects: if really collected, cannot keep them; for userdata
** being finalized, keep them in keys, but not in values
*/
static int iscleared(const Value *o, int iskey) {
  if (!IS_COLLECTABLE(o)) {
    return 0;
  }
  if (IS_TYPE_STRING(o)) {
    stringmark(
        RAW_STRING_VALUE(o)); /* strings are `values', so are never weak */
    return 0;
  }
  return iswhite(GC_VALUE(o)) ||
         (IS_TYPE_USERDATA(o) && (!iskey && isfinalized(USERDATA_VALUE(o))));
}

/*
** clear collected entries from weaktables
*/
static void cleartable(GCObject *l) {
  while (l) {
    Table *h = gco2h(l);
    int i = h->sizearray;
    DEBUG_ASSERT(testbit(h->header.marked, VALUEWEAKBIT) ||
                 testbit(h->header.marked, KEYWEAKBIT));
    if (testbit(h->header.marked, VALUEWEAKBIT)) {
      while (i--) {
        Value *o = &h->array[i];
        if (iscleared(o, 0)) { /* value was collected? */
          SET_NIL(o);          /* remove value */
        }
      }
    }
    i = sizenode(h);
    while (i--) {
      Node *n = gnode(h, i);
      if (!IS_TYPE_NIL(gval(n)) && /* non-empty entry? */
          (iscleared(key2tval(n), 1) || iscleared(gval(n), 0))) {
        SET_NIL(gval(n)); /* remove value ... */
        removeentry(n);   /* remove entry from table */
      }
    }
    l = h->gclist;
  }
}

static void freeobj(lua_State *L, GCObject *o) {
  switch (o->gch.tt) {
  case LUA_TYPE_PROTO:
    luaF_freeproto(L, gco2p(o));
    break;
  case LUA_TYPE_FUNCTION:
    luaF_freeclosure(L, gco2cl(o));
    break;
  case LUA_TYPE_UPVALUE:
    luaF_freeupval(L, gco2uv(o));
    break;
  case LUA_TYPE_TABLE:
    luaH_free(L, gco2h(o));
    break;
  case LUA_TYPE_THREAD: {
    DEBUG_ASSERT(gco2th(o) != L && gco2th(o) != G(L)->mainthread);
    luaE_freethread(L, gco2th(o));
    break;
  }
  case LUA_TYPE_STRING: {
    G(L)->strt.nuse--;
    luaM_freemem(L, o, sizestring(gco2ts(o)));
    break;
  }
  case LUA_TYPE_USERDATA: {
    luaM_freemem(L, o, sizeudata(gco2u(o)));
    break;
  }
  default:
    DEBUG_ASSERT(0);
  }
}

#define sweepwholelist(L, p) sweeplist(L, p, MAX_LUMEM)

static GCObject **sweeplist(lua_State *L, GCObject **p, lu_mem count) {
  GCObject *curr;
  global_State *g = G(L);
  int deadmask = otherwhite(g);
  while ((curr = *p) != NULL && count-- > 0) {
    if (curr->gch.tt ==
        LUA_TYPE_THREAD) { /* sweep open upvalues of each thread */
      sweepwholelist(L, &gco2th(curr)->openUpval);
    }
    if ((curr->gch.marked ^ WHITEBITS) & deadmask) { /* not dead? */
      DEBUG_ASSERT(!IS_DEAD(g, curr) || testbit(curr->gch.marked, FIXEDBIT));
      makewhite(g, curr); /* make it white (for next cycle) */
      p = &curr->gch.next;
    } else { /* must erase `curr' */
      DEBUG_ASSERT(IS_DEAD(g, curr) || deadmask == bitmask(SFIXEDBIT));
      *p = curr->gch.next;
      if (curr == g->rootgc) {      /* is the first element of the list? */
        g->rootgc = curr->gch.next; /* adjust first */
      }
      freeobj(L, curr);
    }
  }
  return p;
}

static void checkSizes(lua_State *L) {
  global_State *g = G(L);
  /* check size of string hash */
  if (g->strt.nuse < cast(lu_int32, g->strt.size / 4) &&
      g->strt.size > MINSTRTABSIZE * 2) {
    String_resize(L, g->strt.size / 2); /* table is too big */
  }
  /* check size of buffer */
  if (luaZ_sizebuffer(&g->buff) > LUA_MINBUFFER * 2) { /* buffer too big? */
    size_t newsize = luaZ_sizebuffer(&g->buff) / 2;
    luaZ_resizebuffer(L, &g->buff, newsize);
  }
}

static void GCTM(lua_State *L) {
  global_State *g = G(L);
  GCObject *o = g->tmudata->gch.next; /* get first element */
  Userdata *udata = rawgco2u(o);
  const Value *tm;
  /* remove udata from `tmudata' */
  if (o == g->tmudata) { /* last element? */
    g->tmudata = nullptr;
  } else {
    g->tmudata->gch.next = udata->uv.header.next;
  }
  // Return it to 'root' list.
  udata->uv.header.next = g->mainthread->header.next;
  g->mainthread->header.next = o;
  makewhite(g, o);
  tm = fasttm(L, udata->uv.metatable, TM_GC);
  if (tm != nullptr) {
    uint8_t oldah = L->allowHook;
    lu_mem oldt = g->GCthreshold;
    L->allowHook = 0; /* stop debug hooks during GC tag method */
    g->GCthreshold = 2 * g->totalbytes; /* avoid GC steps */
    SET_OBJECT_TO_STACK(L, L->top, tm);
    SET_USERDATA(L, L->top + 1, udata);
    L->top += 2;
    luaD_call(L, L->top - 2, 0);
    L->allowHook = oldah;  /* restore hooks */
    g->GCthreshold = oldt; /* restore threshold */
  }
}

/*
** Call all GC tag methods
*/
void luaC_callGCTM(lua_State *L) {
  while (G(L)->tmudata) {
    GCTM(L);
  }
}

void luaC_freeall(lua_State *L) {
  global_State *g = G(L);
  int i;
  g->currentwhite =
      WHITEBITS | bitmask(SFIXEDBIT); /* mask to collect all elements */
  sweepwholelist(L, &g->rootgc);
  for (i = 0; i < g->strt.size; i++) { /* free all string lists */
    sweepwholelist(L, &g->strt.hash[i]);
  }
}

static void markmt(global_State *g) {
  int i;
  for (i = 0; i < NUM_TYPES; i++) {
    if (g->mt[i]) {
      markobject(g, g->mt[i]);
    }
  }
}

/* mark root set */
static void markroot(lua_State *L) {
  global_State *g = G(L);
  g->gray = nullptr;
  g->grayagain = nullptr;
  g->weak = nullptr;
  markobject(g, g->mainthread);
  /* make global table be traversed before main stack */
  markvalue(g, gt(g->mainthread));
  markvalue(g, registry(L));
  markmt(g);
  g->gcstate = GCSpropagate;
}

static void remarkupvals(global_State *g) {
  Upvalue *uv;
  for (uv = g->uvhead.u.l.next; uv != &g->uvhead; uv = uv->u.l.next) {
    DEBUG_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
    if (isgray(LuaObjectToGCObject(uv)))
      markvalue(g, uv->v);
  }
}

static void atomic(lua_State *L) {
  global_State *g = G(L);
  size_t udsize; /* total size of userdata to be finalized */
  /* remark occasional upvalues of (maybe) dead threads */
  remarkupvals(g);
  /* traverse objects cautch by write barrier and by 'remarkupvals' */
  propagateall(g);
  /* remark weak tables */
  g->gray = g->weak;
  g->weak = nullptr;
  DEBUG_ASSERT(!iswhite(LuaObjectToGCObject(g->mainthread)));
  markobject(g, L); /* mark running thread */
  markmt(g);        /* mark basic metatables (again) */
  propagateall(g);
  /* remark gray again */
  g->gray = g->grayagain;
  g->grayagain = nullptr;
  propagateall(g);
  udsize = luaC_separateudata(L, 0); /* separate userdata to be finalized */
  marktmu(g);                        /* mark `preserved' userdata */
  udsize += propagateall(g);         /* remark, to propagate `preserveness' */
  cleartable(g->weak); /* remove collected objects from weak tables */
  /* flip current white */
  g->currentwhite = cast_byte(otherwhite(g));
  g->sweepstrgc = 0;
  g->sweepgc = &g->rootgc;
  g->gcstate = GCSsweepstring;
  g->estimate = g->totalbytes - udsize; /* first estimate */
}

static l_mem singlestep(lua_State *L) {
  global_State *g = G(L);
  /*lua_checkmemory(L);*/
  switch (g->gcstate) {
  case GCSpause: {
    markroot(L); /* start a new collection */
    return 0;
  }
  case GCSpropagate: {
    if (g->gray) {
      return propagatemark(g);
    } else {     /* no more `gray' objects */
      atomic(L); /* finish mark phase */
      return 0;
    }
  }
  case GCSsweepstring: {
    lu_mem old = g->totalbytes;
    sweepwholelist(L, &g->strt.hash[g->sweepstrgc++]);
    if (g->sweepstrgc >= g->strt.size) { /* nothing more to sweep? */
      g->gcstate = GCSsweep;             /* end sweep-string phase */
    }
    DEBUG_ASSERT(old >= g->totalbytes);
    g->estimate -= old - g->totalbytes;
    return GCSWEEPCOST;
  }
  case GCSsweep: {
    lu_mem old = g->totalbytes;
    g->sweepgc = sweeplist(L, g->sweepgc, GCSWEEPMAX);
    if (*g->sweepgc == NULL) { /* nothing more to sweep? */
      checkSizes(L);
      g->gcstate = GCSfinalize; /* end sweep phase */
    }
    DEBUG_ASSERT(old >= g->totalbytes);
    g->estimate -= old - g->totalbytes;
    return GCSWEEPMAX * GCSWEEPCOST;
  }
  case GCSfinalize: {
    if (g->tmudata) {
      GCTM(L);
      if (g->estimate > GCFINALIZECOST) {
        g->estimate -= GCFINALIZECOST;
      }
      return GCFINALIZECOST;
    } else {
      g->gcstate = GCSpause; /* end collection */
      g->gcdept = 0;
      return 0;
    }
  }
  default:
    DEBUG_ASSERT(0);
    return 0;
  }
}

void luaC_step(lua_State *L) {
  global_State *g = G(L);
  l_mem lim = (GCSTEPSIZE / 100) * g->gcstepmul;
  if (lim == 0) {
    lim = (MAX_LUMEM - 1) / 2; /* no limit */
  }
  g->gcdept += g->totalbytes - g->GCthreshold;
  do {
    lim -= singlestep(L);
    if (g->gcstate == GCSpause) {
      break;
    }
  } while (lim > 0);
  if (g->gcstate != GCSpause) {
    if (g->gcdept < GCSTEPSIZE) {
      g->GCthreshold = g->totalbytes + GCSTEPSIZE; /* - lim/g->gcstepmul;*/
    } else {
      g->gcdept -= GCSTEPSIZE;
      g->GCthreshold = g->totalbytes;
    }
  } else {
    setthreshold(g);
  }
}

void luaC_fullgc(lua_State *L) {
  global_State *g = G(L);
  if (g->gcstate <= GCSpropagate) {
    /* reset sweep marks to sweep all elements (returning them to white) */
    g->sweepstrgc = 0;
    g->sweepgc = &g->rootgc;
    /* reset other collector lists */
    g->gray = nullptr;
    g->grayagain = nullptr;
    g->weak = nullptr;
    g->gcstate = GCSsweepstring;
  }
  DEBUG_ASSERT(g->gcstate != GCSpause && g->gcstate != GCSpropagate);
  /* finish any pending sweep phase */
  while (g->gcstate != GCSfinalize) {
    DEBUG_ASSERT(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep);
    singlestep(L);
  }
  markroot(L);
  while (g->gcstate != GCSpause) {
    singlestep(L);
  }
  setthreshold(g);
}

void luaC_barrierf(lua_State *L, GCObject *o, GCObject *v) {
  global_State *g = G(L);
  DEBUG_ASSERT(isblack(o) && iswhite(v) && !IS_DEAD(g, v) && !IS_DEAD(g, o));
  DEBUG_ASSERT(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
  DEBUG_ASSERT(GET_TYPE(&o->gch) != LUA_TYPE_TABLE);
  /* must keep invariant? */
  if (g->gcstate == GCSpropagate) {
    reallymarkobject(g, v); /* restore invariant */
  } else {                  /* don't mind */
    makewhite(g, o);        /* mark as white just to avoid other barriers */
  }
}

void luaC_barrierback(lua_State *L, Table *t) {
  global_State *g = G(L);
  GCObject *o = LuaObjectToGCObject(t);
  DEBUG_ASSERT(isblack(o) && !IS_DEAD(g, o));
  DEBUG_ASSERT(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
  black2gray(o); /* make table gray (again) */
  t->gclist = g->grayagain;
  g->grayagain = o;
}

void luaC_link(lua_State *L, GCObject *o, uint8_t tt) {
  global_State *g = G(L);
  o->gch.next = g->rootgc;
  g->rootgc = o;
  o->gch.marked = luaC_white(g);
  o->gch.tt = tt;
}

void luaC_linkupval(lua_State *L, Upvalue *uv) {
  global_State *g = G(L);
  GCObject *o = LuaObjectToGCObject(uv);
  o->gch.next = g->rootgc; /* link upvalue into `rootgc' list */
  g->rootgc = o;
  if (isgray(o)) {
    if (g->gcstate == GCSpropagate) {
      gray2black(o); /* closed upvalues need barrier */
      luaC_barrier(L, uv, uv->v);
    } else { /* sweep phase: sweep it (turning it into white) */
      makewhite(g, o);
      DEBUG_ASSERT(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
    }
  }
}
