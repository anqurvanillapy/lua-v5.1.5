/* Auxiliary functions to manipulate prototypes and closures. */

#include <stddef.h>

#include "lua.h"

#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lstate.h"
#include "object.h"

Closure *luaF_newCclosure(lua_State *L, int nelems, Table *e) {
  Closure *c = cast(Closure *, luaM_malloc(L, sizeCclosure(nelems)));
  luaC_link(L, LuaObjectToGCObject(c), LUA_TYPE_FUNCTION);
  c->c.header.isC = true;
  c->c.header.env = e;
  c->c.header.nupvalues = (uint8_t)nelems;
  return c;
}

Closure *luaF_newLclosure(lua_State *L, int nelems, Table *e) {
  Closure *c = cast(Closure *, luaM_malloc(L, sizeLclosure(nelems)));
  luaC_link(L, LuaObjectToGCObject(c), LUA_TYPE_FUNCTION);
  c->l.header.isC = false;
  c->l.header.env = e;
  c->l.header.nupvalues = (uint8_t)nelems;
  while (nelems--) {
    c->l.upvalues[nelems] = nullptr;
  }
  return c;
}

Upvalue *luaF_newupval(lua_State *L) {
  Upvalue *uv = luaM_new(L, Upvalue);
  luaC_link(L, LuaObjectToGCObject(uv), LUA_TYPE_UPVALUE);
  uv->v = &uv->u.value;
  SET_NIL(uv->v);
  return uv;
}

Upvalue *luaF_findupval(lua_State *L, StackIndex level) {
  GlobalState *g = G(L);
  GCObject **pp = &L->openUpval;
  Upvalue *p;
  Upvalue *uv;
  while (*pp != nullptr && (p = ngcotouv(*pp))->v >= level) {
    DEBUG_ASSERT(p->v != &p->u.value);
    if (p->v == level) { /* found a corresponding upvalue? */
      if (IS_DEAD(g, LuaObjectToGCObject(p))) { /* is it dead? */
        changewhite(LuaObjectToGCObject(p));    /* ressurect it */
      }
      return p;
    }
    pp = &p->header.next;
  }
  uv = luaM_new(L, Upvalue); /* not found: create a new one */
  uv->header.tt = LUA_TYPE_UPVALUE;
  uv->header.marked = luaC_white(g);
  uv->v = level;         /* current value lives in the stack */
  uv->header.next = *pp; /* chain it in the proper position */
  *pp = LuaObjectToGCObject(uv);
  uv->u.l.prev = &g->uvhead; /* double link it in `uvhead' list */
  uv->u.l.next = g->uvhead.u.l.next;
  uv->u.l.next->u.l.prev = uv;
  g->uvhead.u.l.next = uv;
  DEBUG_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
  return uv;
}

static void unlinkupval(Upvalue *uv) {
  DEBUG_ASSERT(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
  uv->u.l.next->u.l.prev = uv->u.l.prev; /* remove from `uvhead' list */
  uv->u.l.prev->u.l.next = uv->u.l.next;
}

void luaF_freeupval(lua_State *L, Upvalue *uv) {
  if (uv->v != &uv->u.value) { /* is it open? */
    unlinkupval(uv);           /* remove from open list */
  }
  luaM_free(L, uv); /* free upvalue */
}

void luaF_close(lua_State *L, StackIndex level) {
  Upvalue *uv;
  GlobalState *g = G(L);
  while (L->openUpval != nullptr && (uv = ngcotouv(L->openUpval))->v >= level) {
    GCObject *o = LuaObjectToGCObject(uv);
    DEBUG_ASSERT(!isblack(o) && uv->v != &uv->u.value);
    L->openUpval = uv->header.next; /* remove from `open' list */
    if (IS_DEAD(g, o)) {
      luaF_freeupval(L, uv); /* free upvalue */
    } else {
      unlinkupval(uv);
      SET_OBJECT(L, &uv->u.value, uv->v);
      uv->v = &uv->u.value;  /* now current value lives here */
      luaC_linkupval(L, uv); /* link upvalue into `gcroot' list */
    }
  }
}

Prototype *luaF_newproto(lua_State *L) {
  Prototype *f = luaM_new(L, Prototype);
  luaC_link(L, LuaObjectToGCObject(f), LUA_TYPE_PROTO);
  f->k = nullptr;
  f->kSize = 0;
  f->inners = nullptr;
  f->pSize = 0;
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

void luaF_freeproto(lua_State *L, Prototype *f) {
  luaM_freeArray(L, f->code, f->codeSize, Instruction);
  luaM_freeArray(L, f->inners, f->pSize, Prototype *);
  luaM_freeArray(L, f->k, f->kSize, Value);
  luaM_freeArray(L, f->lineInfo, f->lineInfoSize, int);
  luaM_freeArray(L, f->locVars, f->locVarsSize, struct LocVar);
  luaM_freeArray(L, f->upvalues, f->upvaluesSize, String *);
  luaM_free(L, f);
}

void luaF_freeclosure(lua_State *L, Closure *c) {
  int size = (c->c.header.isC) ? sizeCclosure(c->c.header.nupvalues)
                               : sizeLclosure(c->l.header.nupvalues);
  luaM_freemem(L, c, size);
}

/*
** Look for n-th local variable at line `line` in function `func`.
** Returns nullptr if not found.
*/
const char *luaF_getlocalname(const Prototype *f, int local_number, int pc) {
  int i;
  for (i = 0; i < f->locVarsSize && f->locVars[i].startPC <= pc; i++) {
    if (pc < f->locVars[i].endPC) { /* is variable active? */
      local_number--;
      if (local_number == 0) {
        return STRING_CONTENT(f->locVars[i].varname);
      }
    }
  }
  return nullptr; /* not found */
}
