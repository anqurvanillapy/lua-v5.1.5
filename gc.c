#include <string.h>

#include "lua.h"

#include "closure.h"
#include "gc.h"
#include "intern.h"
#include "memory.h"
#include "object.h"
#include "stack.h"
#include "state.h"
#include "table.h"
#include "tag.h"

#define GCSTEPSIZE 1024u
#define GCSWEEPMAX 40
#define GCSWEEPCOST 10
#define GCFINALIZECOST 100

#define maskmarks (uint8_t)(~(bitmask(BLACKBIT) | WHITEBITS))

#define makewhite(g, x)                                                        \
  ((x)->gch.marked = (uint8_t)(((x)->gch.marked & maskmarks) | luaC_white(g)))

#define white2gray(x) reset2bits((x)->gch.marked, WHITE0BIT, WHITE1BIT)
#define black2gray(x) resetbit((x)->gch.marked, BLACKBIT)

#define stringmark(s) reset2bits((s)->header.marked, WHITE0BIT, WHITE1BIT)

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
  assert(IS_TYPE_NIL(gval(n)));
  if (IS_COLLECTABLE(gkey(n))) {
    SET_TYPE(gkey(n), LUA_TYPE_DEAD); /* dead key; remove it */
  }
}

static void reallymarkobject(GlobalState *g, GCObject *o) {
  assert(iswhite(o) && !IS_DEAD(g, o));
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
    assert(0);
  }
}

static void marktmu(GlobalState *g) {
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
  GlobalState *g = G(L);
  size_t deadmem = 0;
  GCObject **p = &g->mainthread->header.next;
  GCObject *curr;
  while ((curr = *p) != NULL) {
    if (!(iswhite(curr) || all) || isfinalized(gco2u(curr))) {
      p = &curr->gch.next; /* don't bother with them */
    } else if (FAST_TM(L, gco2u(curr)->metatable, TM_GC) == NULL) {
      markfinalized(gco2u(curr)); /* don't need finalization */
      p = &curr->gch.next;
    } else { /* must call its gc method */
      deadmem += USERDATA_SIZE(gco2u(curr));
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

static int traversetable(GlobalState *g, Table *h) {
  int i;
  int weakkey = 0;
  int weakvalue = 0;
  const Value *mode;
  if (h->metatable)
    markobject(g, h->metatable);
  mode = GLOBAL_FAST_TM(g, h->metatable, TM_MODE);
  if (mode && IS_TYPE_STRING(mode)) { /* is there a weak mode? */
    weakkey = (strchr(VALUE_STRING_CONTENT(mode), 'k') != NULL);
    weakvalue = (strchr(VALUE_STRING_CONTENT(mode), 'v') != NULL);
    if (weakkey || weakvalue) {                   /* is really weak? */
      h->header.marked &= ~(KEYWEAK | VALUEWEAK); /* clear bits */
      h->header.marked |=
          (uint8_t)((weakkey << KEYWEAKBIT) | (weakvalue << VALUEWEAKBIT));
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
    assert(GET_TYPE(gkey(n)) != LUA_TYPE_DEAD || IS_TYPE_NIL(gval(n)));
    if (IS_TYPE_NIL(gval(n))) {
      removeentry(n); /* remove empty entries */
    } else {
      assert(!IS_TYPE_NIL(gkey(n)));
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
static void traverseproto(GlobalState *g, Prototype *f) {
  if (f->source) {
    stringmark(f->source);
  }
  for (size_t i = 0; i < f->constantsSize; i++) /* mark literals */
    markvalue(g, &f->constants[i]);
  for (size_t i = 0; i < f->upvaluesSize; i++) { /* mark upvalue names */
    if (f->upvalues[i]) {
      stringmark(f->upvalues[i]);
    }
  }
  for (size_t i = 0; i < f->innersSize; i++) { /* mark nested protos */
    if (f->inners[i])
      markobject(g, f->inners[i]);
  }
  for (size_t i = 0; i < f->locVarsSize; i++) { /* mark local-variable names */
    if (f->locVars[i].name) {
      stringmark(f->locVars[i].name);
    }
  }
}

static void traverseclosure(GlobalState *g, Closure *cl) {
  markobject(g, cl->c.header.env);
  if (cl->c.header.isC) {
    for (size_t i = 0; i < cl->c.header.nupvalues; i++) {
      // Mark its upvalues.
      markvalue(g, &cl->c.upvalue[i]);
    }
  } else {
    assert(cl->l.header.nupvalues == cl->l.p->upvaluesNum);
    markobject(g, cl->l.p);
    for (size_t i = 0; i < cl->l.header.nupvalues; i++) {
      // Mark its upvalues.
      markobject(g, cl->l.upvalues[i]);
    }
  }
}

static void checkstacksizes(lua_State *L, StackIndex max) {
  int ci_used = (int)(L->ci - L->baseCI); /* number of `ci' in use */
  int s_used = (int)(max - L->stack);     /* part of stack in use */
  if (L->ciSize > LUAI_MAXCALLS) {        /* handling overflow? */
    return;                               /* do not touch the stacks */
  }
  if (4 * ci_used < L->ciSize && 2 * BASIC_CI_SIZE < L->ciSize) {
    Stack_resizeCI(L, L->ciSize / 2); /* still big enough... */
  }
  condhardstacktests(Stack_resizeCI(L, ci_used + 1));
  if (4 * s_used < L->stackSize &&
      2 * (BASIC_STACK_SIZE + EXTRA_STACK) < L->stackSize) {
    Stack_resize(L, L->stackSize / 2); /* still big enough... */
  }
  condhardstacktests(Stack_resize(L, s_used));
}

static void traversestack(GlobalState *g, lua_State *l) {
  StackIndex o, lim;
  CallInfo *ci;
  markvalue(g, GLOBALS(l));
  lim = l->top;
  for (ci = l->baseCI; ci <= l->ci; ci++) {
    assert(ci->top <= l->stackLast);
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
static ptrdiff_t propagatemark(GlobalState *g) {
  GCObject *o = g->gray;
  assert(isgray(o));
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
    return (cl->c.header.isC) ? C_CLOSURE_SIZE(cl->c.header.nupvalues)
                              : L_CLOSURE_SIZE(cl->l.header.nupvalues);
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
           sizeof(Prototype *) * p->innersSize +
           sizeof(Value) * p->constantsSize + sizeof(int) * p->lineInfoSize +
           sizeof(LocVar) * p->locVarsSize + sizeof(String *) * p->upvaluesSize;
  }
  default:
    assert(0);
    return 0;
  }
}

static size_t propagateall(GlobalState *g) {
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
    stringmark(STRING_VALUE(o)); /* strings are `values', so are never weak */
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
    assert(testbit(h->header.marked, VALUEWEAKBIT) ||
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
    Prototype_free(L, gco2p(o));
    break;
  case LUA_TYPE_FUNCTION:
    Closure_free(L, gco2cl(o));
    break;
  case LUA_TYPE_UPVALUE:
    Upvalue_free(L, gco2uv(o));
    break;
  case LUA_TYPE_TABLE:
    Table_free(L, gco2h(o));
    break;
  case LUA_TYPE_THREAD: {
    assert(gco2th(o) != L && gco2th(o) != G(L)->mainthread);
    State_freeThread(L, gco2th(o));
    break;
  }
  case LUA_TYPE_STRING: {
    G(L)->pool.itemsNum--;
    Mem_free(L, o, STRING_SIZE(gco2ts(o)));
    break;
  }
  case LUA_TYPE_USERDATA: {
    Mem_free(L, o, USERDATA_SIZE(gco2u(o)));
    break;
  }
  default:
    assert(0);
  }
}

#define sweepwholelist(L, p) sweeplist(L, p, SIZE_MAX)

static GCObject **sweeplist(lua_State *L, GCObject **p, size_t count) {
  GCObject *curr;
  GlobalState *g = G(L);
  int deadmask = otherwhite(g);
  while ((curr = *p) != NULL && count-- > 0) {
    if (curr->gch.tt ==
        LUA_TYPE_THREAD) { /* sweep open upvalues of each thread */
      sweepwholelist(L, &gco2th(curr)->openUpval);
    }
    if ((curr->gch.marked ^ WHITEBITS) & deadmask) { /* not dead? */
      assert(!IS_DEAD(g, curr) || testbit(curr->gch.marked, FIXEDBIT));
      makewhite(g, curr); /* make it white (for next cycle) */
      p = &curr->gch.next;
    } else { /* must erase `curr' */
      assert(IS_DEAD(g, curr) || deadmask == bitmask(SFIXEDBIT));
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
  GlobalState *g = G(L);
  /* check size of string pool buckets */
  if (g->pool.itemsNum < (uint32_t)(g->pool.bucketsSize / 4) &&
      g->pool.bucketsSize > MINSTRTABSIZE * 2) {
    StringPool_resize(L, g->pool.bucketsSize / 2); /* table is too big */
  }
  /* check size of buffer */
  if (StringBuilder_size(&g->buff) >
      LUA_MIN_BUF_SIZE * 2) { /* buffer too big? */
    size_t newsize = StringBuilder_size(&g->buff) / 2;
    StringBuilder_resize(L, &g->buff, newsize);
  }
}

static void GCTM(lua_State *L) {
  GlobalState *g = G(L);
  GCObject *o = g->tmudata->gch.next; /* get first element */
  Userdata *udata = gco2u(o);
  const Value *tm;
  /* remove udata from `tmudata' */
  if (o == g->tmudata) { /* last element? */
    g->tmudata = nullptr;
  } else {
    g->tmudata->gch.next = udata->header.next;
  }
  // Return it to 'root' list.
  udata->header.next = g->mainthread->header.next;
  g->mainthread->header.next = o;
  makewhite(g, o);
  tm = FAST_TM(L, udata->metatable, TM_GC);
  if (tm != nullptr) {
    bool allowHook = L->allowHook;
    size_t oldt = g->GCthreshold;
    L->allowHook = false;               // stop debug hooks during GC tag method
    g->GCthreshold = 2 * g->totalbytes; /* avoid GC steps */
    SET_OBJECT_TO_STACK(L, L->top, tm);
    SET_USERDATA(L, L->top + 1, udata);
    L->top += 2;
    luaD_call(L, L->top - 2, 0);
    L->allowHook = allowHook; /* restore hooks */
    g->GCthreshold = oldt;    /* restore threshold */
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
  GlobalState *g = G(L);
  // Mask to collect all elements.
  g->currentwhite = WHITEBITS | bitmask(SFIXEDBIT);
  sweepwholelist(L, &g->rootgc);
  for (size_t i = 0; i < g->pool.bucketsSize; i++) {
    // Free all string lists.
    sweepwholelist(L, &g->pool.buckets[i]);
  }
}

static void markmt(GlobalState *g) {
  int i;
  for (i = 0; i < NUM_TYPES; i++) {
    if (g->mt[i]) {
      markobject(g, g->mt[i]);
    }
  }
}

/* mark root set */
static void markroot(lua_State *L) {
  GlobalState *g = G(L);
  g->gray = nullptr;
  g->grayagain = nullptr;
  g->weak = nullptr;
  markobject(g, g->mainthread);
  /* make global table be traversed before main stack */
  markvalue(g, GLOBALS(g->mainthread));
  markvalue(g, REGISTRY(L));
  markmt(g);
  g->gcstate = GCSpropagate;
}

static void remarkupvals(GlobalState *g) {
  Upvalue *uv;
  for (uv = g->uvhead.u.l.next; uv != &g->uvhead; uv = uv->u.l.next) {
    assert(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
    if (isgray(LuaObjectToGCObject(uv)))
      markvalue(g, uv->v);
  }
}

static void atomic(lua_State *L) {
  GlobalState *g = G(L);
  size_t udsize; /* total size of userdata to be finalized */
  /* remark occasional upvalues of (maybe) dead threads */
  remarkupvals(g);
  /* traverse objects cautch by write barrier and by 'remarkupvals' */
  propagateall(g);
  /* remark weak tables */
  g->gray = g->weak;
  g->weak = nullptr;
  assert(!iswhite(LuaObjectToGCObject(g->mainthread)));
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
  g->currentwhite = (uint8_t)otherwhite(g);
  g->sweepstrgc = 0;
  g->sweepgc = &g->rootgc;
  g->gcstate = GCSsweepstring;
  g->estimate = g->totalbytes - udsize; /* first estimate */
}

static ptrdiff_t singlestep(lua_State *L) {
  GlobalState *g = G(L);
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
    size_t old = g->totalbytes;
    sweepwholelist(L, &g->pool.buckets[g->sweepstrgc++]);
    if (g->sweepstrgc >= g->pool.bucketsSize) { /* nothing more to sweep? */
      g->gcstate = GCSsweep;                    /* end sweep-string phase */
    }
    assert(old >= g->totalbytes);
    g->estimate -= old - g->totalbytes;
    return GCSWEEPCOST;
  }
  case GCSsweep: {
    size_t old = g->totalbytes;
    g->sweepgc = sweeplist(L, g->sweepgc, GCSWEEPMAX);
    if (*g->sweepgc == NULL) { /* nothing more to sweep? */
      checkSizes(L);
      g->gcstate = GCSfinalize; /* end sweep phase */
    }
    assert(old >= g->totalbytes);
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
    assert(0);
    return 0;
  }
}

void luaC_step(lua_State *L) {
  GlobalState *g = G(L);
  ptrdiff_t lim = (GCSTEPSIZE / 100) * g->gcstepmul;
  if (lim == 0) {
    lim = (SIZE_MAX - 1) / 2; /* no limit */
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
  GlobalState *g = G(L);
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
  assert(g->gcstate != GCSpause && g->gcstate != GCSpropagate);
  /* finish any pending sweep phase */
  while (g->gcstate != GCSfinalize) {
    assert(g->gcstate == GCSsweepstring || g->gcstate == GCSsweep);
    singlestep(L);
  }
  markroot(L);
  while (g->gcstate != GCSpause) {
    singlestep(L);
  }
  setthreshold(g);
}

void luaC_barrierf(lua_State *L, GCObject *o, GCObject *v) {
  GlobalState *g = G(L);
  assert(isblack(o) && iswhite(v) && !IS_DEAD(g, v) && !IS_DEAD(g, o));
  assert(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
  assert(GET_TYPE(&o->gch) != LUA_TYPE_TABLE);
  /* must keep invariant? */
  if (g->gcstate == GCSpropagate) {
    reallymarkobject(g, v); /* restore invariant */
  } else {                  /* don't mind */
    makewhite(g, o);        /* mark as white just to avoid other barriers */
  }
}

void luaC_barrierback(lua_State *L, Table *t) {
  GlobalState *g = G(L);
  GCObject *o = LuaObjectToGCObject(t);
  assert(isblack(o) && !IS_DEAD(g, o));
  assert(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
  black2gray(o); /* make table gray (again) */
  t->gclist = g->grayagain;
  g->grayagain = o;
}

void luaC_link(lua_State *L, GCObject *o, uint8_t tt) {
  GlobalState *g = G(L);
  o->gch.next = g->rootgc;
  g->rootgc = o;
  o->gch.marked = luaC_white(g);
  o->gch.tt = tt;
}

void luaC_linkupval(lua_State *L, Upvalue *uv) {
  GlobalState *g = G(L);
  GCObject *o = LuaObjectToGCObject(uv);
  o->gch.next = g->rootgc; /* link upvalue into `rootgc' list */
  g->rootgc = o;
  if (isgray(o)) {
    if (g->gcstate == GCSpropagate) {
      gray2black(o); /* closed upvalues need barrier */
      luaC_barrier(L, uv, uv->v);
    } else { /* sweep phase: sweep it (turning it into white) */
      makewhite(g, o);
      assert(g->gcstate != GCSfinalize && g->gcstate != GCSpause);
    }
  }
}
