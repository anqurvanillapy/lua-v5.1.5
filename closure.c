#include "closure.h"
#include "gc.h"
#include "memory.h"
#include "object.h"
#include "state.h"

Closure *Closure_newC(lua_State *L, size_t nelems, Table *e) {
  Closure *c = Mem_alloc(L, C_CLOSURE_SIZE(nelems));
  luaC_link(L, LuaObjectToGCObject(c), LUA_TYPE_FUNCTION);
  c->c.header.isC = true;
  c->c.header.env = e;
  c->c.header.nupvalues = nelems;
  return c;
}

Closure *Closure_newL(lua_State *L, size_t nelems, Table *e) {
  Closure *c = Mem_alloc(L, L_CLOSURE_SIZE(nelems));
  luaC_link(L, LuaObjectToGCObject(c), LUA_TYPE_FUNCTION);
  c->l.header.isC = false;
  c->l.header.env = e;
  c->l.header.nupvalues = nelems;
  while (nelems--) {
    c->l.upvalues[nelems] = nullptr;
  }
  return c;
}

Upvalue *Upvalue_new(lua_State *L) {
  Upvalue *uv = Mem_new(L, Upvalue);
  luaC_link(L, LuaObjectToGCObject(uv), LUA_TYPE_UPVALUE);
  uv->v = &uv->u.value;
  SET_NIL(uv->v);
  return uv;
}

Upvalue *Closure_findUpvalue(lua_State *L, StackIndex level) {
  GlobalState *g = G(L);
  GCObject **pp = &L->openUpval;
  Upvalue *p = ngcotouv(*pp);
  while (*pp != nullptr && p->v >= level) {
    assert(p->v != &p->u.value);
    if (p->v == level) {
      if (IS_DEAD(g, LuaObjectToGCObject(p))) {
        // resurrect it if it's dead.
        changewhite(LuaObjectToGCObject(p));
      }
      return p;
    }
    pp = &p->header.next;
    p = ngcotouv(*pp);
  }

  // Not found, create a new one.
  Upvalue *uv = Mem_new(L, Upvalue);
  uv->header.tt = LUA_TYPE_UPVALUE;
  uv->header.marked = luaC_white(g);
  uv->v = level;         // current value lives in the stack
  uv->header.next = *pp; // chain it in the proper position
  *pp = LuaObjectToGCObject(uv);
  uv->u.l.prev = &g->uvhead; // double link it in uvhead list.
  uv->u.l.next = g->uvhead.u.l.next;
  uv->u.l.next->u.l.prev = uv;
  g->uvhead.u.l.next = uv;
  assert(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
  return uv;
}

static void unlinkUpvalue(Upvalue *uv) {
  assert(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
  uv->u.l.next->u.l.prev = uv->u.l.prev; // remove from uvhead list
  uv->u.l.prev->u.l.next = uv->u.l.next;
}

void Upvalue_free(lua_State *L, Upvalue *uv) {
  if (uv->v != &uv->u.value) { /* is it open? */
    unlinkUpvalue(uv);         /* remove from open list */
  }
  Mem_freePtr(L, uv); /* free upvalue */
}

void Closure_close(lua_State *L, StackIndex level) {
  GlobalState *g = G(L);
  Upvalue *uv = ngcotouv(L->openUpval);
  while (L->openUpval != nullptr && uv->v >= level) {
    GCObject *o = LuaObjectToGCObject(uv);
    assert(!isblack(o) && uv->v != &uv->u.value);
    L->openUpval = uv->header.next; /* remove from `open' list */
    if (IS_DEAD(g, o)) {
      Upvalue_free(L, uv); /* free upvalue */
    } else {
      unlinkUpvalue(uv);
      SET_OBJECT(L, &uv->u.value, uv->v);
      uv->v = &uv->u.value;  // now current value lives here
      luaC_linkupval(L, uv); // link upvalue into gcroot list
    }
    uv = ngcotouv(L->openUpval);
  }
}

Prototype *Prototype_new(lua_State *L) {
  Prototype *f = Mem_new(L, Prototype);
  luaC_link(L, LuaObjectToGCObject(f), LUA_TYPE_PROTO);
  f->constants = nullptr;
  f->constantsSize = 0;
  f->inners = nullptr;
  f->innersSize = 0;
  f->code = nullptr;
  f->codeSize = 0;
  f->lineInfoSize = 0;
  f->upvaluesSize = 0;
  f->upvaluesNum = 0;
  f->upvalues = nullptr;
  f->paramsNum = 0;
  f->varargMode = 0;
  f->maxStackSize = 0;
  f->lineInfo = nullptr;
  f->locVarsSize = 0;
  f->locVars = nullptr;
  f->lineDefined = 0;
  f->lineDefinedLast = 0;
  f->source = nullptr;
  return f;
}

void Prototype_free(lua_State *L, Prototype *f) {
  Mem_freeVec(L, f->code, f->codeSize, Instruction);
  Mem_freeVec(L, f->inners, f->innersSize, Prototype *);
  Mem_freeVec(L, f->constants, f->constantsSize, Value);
  Mem_freeVec(L, f->lineInfo, f->lineInfoSize, int);
  Mem_freeVec(L, f->locVars, f->locVarsSize, struct LocVar);
  Mem_freeVec(L, f->upvalues, f->upvaluesSize, String *);
  Mem_freePtr(L, f);
}

void Closure_free(lua_State *L, Closure *c) {
  size_t size = c->c.header.isC ? C_CLOSURE_SIZE(c->c.header.nupvalues)
                                : L_CLOSURE_SIZE(c->l.header.nupvalues);
  Mem_free(L, c, size);
}

/// Look for n-th local variable at line `line` in function `func`. Returns
/// nullptr if not found.
const char *Prototype_getLocalName(const Prototype *f, int localNum, int pc) {
  for (size_t i = 0; i < f->locVarsSize && f->locVars[i].startPC <= pc; i++) {
    if (pc < f->locVars[i].endPC) {
      // Variable is active.
      localNum--;
      if (localNum == 0) {
        return STRING_CONTENT(f->locVars[i].name);
      }
    }
  }
  return nullptr;
}
