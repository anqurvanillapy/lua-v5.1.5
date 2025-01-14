/* Auxiliary functions to manipulate prototypes and closures. */

#include <stddef.h>

#include "lua.h"

#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"

Closure *luaF_newCclosure(lua_State *L, int nelems, Table *e) {
  Closure *c = cast(Closure *, luaM_malloc(L, sizeCclosure(nelems)));
  luaC_link(L, LuaObjectToGCObject(c), LUA_TYPE_FUNCTION);
  c->c.isC = 1;
  c->c.env = e;
  c->c.nupvalues = cast_byte(nelems);
  return c;
}

Closure *luaF_newLclosure(lua_State *L, int nelems, Table *e) {
  Closure *c = cast(Closure *, luaM_malloc(L, sizeLclosure(nelems)));
  luaC_link(L, LuaObjectToGCObject(c), LUA_TYPE_FUNCTION);
  c->l.isC = 0;
  c->l.env = e;
  c->l.nupvalues = cast_byte(nelems);
  while (nelems--) {
    c->l.upvalues[nelems] = nullptr;
  }
  return c;
}

UpVal *luaF_newupval(lua_State *L) {
  UpVal *uv = luaM_new(L, UpVal);
  luaC_link(L, LuaObjectToGCObject(uv), LUA_TYPE_UPVALUE);
  uv->v = &uv->u.value;
  SET_NIL(uv->v);
  return uv;
}

UpVal *luaF_findupval(lua_State *L, StkId level) {
  global_State *g = G(L);
  GCObject **pp = &L->openUpval;
  UpVal *p;
  UpVal *uv;
  while (*pp != nullptr && (p = ngcotouv(*pp))->v >= level) {
    lua_assert(p->v != &p->u.value);
    if (p->v == level) { /* found a corresponding upvalue? */
      if (IS_DEAD(g, LuaObjectToGCObject(p))) { /* is it dead? */
        changewhite(LuaObjectToGCObject(p));    /* ressurect it */
      }
      return p;
    }
    pp = &p->next;
  }
  uv = luaM_new(L, UpVal); /* not found: create a new one */
  uv->tt = LUA_TYPE_UPVALUE;
  uv->marked = luaC_white(g);
  uv->v = level;  /* current value lives in the stack */
  uv->next = *pp; /* chain it in the proper position */
  *pp = LuaObjectToGCObject(uv);
  uv->u.l.prev = &g->uvhead; /* double link it in `uvhead' list */
  uv->u.l.next = g->uvhead.u.l.next;
  uv->u.l.next->u.l.prev = uv;
  g->uvhead.u.l.next = uv;
  lua_assert(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
  return uv;
}

static void unlinkupval(UpVal *uv) {
  lua_assert(uv->u.l.next->u.l.prev == uv && uv->u.l.prev->u.l.next == uv);
  uv->u.l.next->u.l.prev = uv->u.l.prev; /* remove from `uvhead' list */
  uv->u.l.prev->u.l.next = uv->u.l.next;
}

void luaF_freeupval(lua_State *L, UpVal *uv) {
  if (uv->v != &uv->u.value) { /* is it open? */
    unlinkupval(uv);           /* remove from open list */
  }
  luaM_free(L, uv); /* free upvalue */
}

void luaF_close(lua_State *L, StkId level) {
  UpVal *uv;
  global_State *g = G(L);
  while (L->openUpval != nullptr && (uv = ngcotouv(L->openUpval))->v >= level) {
    GCObject *o = LuaObjectToGCObject(uv);
    lua_assert(!isblack(o) && uv->v != &uv->u.value);
    L->openUpval = uv->next; /* remove from `open' list */
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

Proto *luaF_newproto(lua_State *L) {
  Proto *f = luaM_new(L, Proto);
  luaC_link(L, LuaObjectToGCObject(f), LUA_TYPE_PROTO);
  f->k = nullptr;
  f->kSize = 0;
  f->inners = nullptr;
  f->pSize = 0;
  f->code = nullptr;
  f->codeSize = 0;
  f->lineInfoSize = 0;
  f->upvaluesSize = 0;
  f->upvalueNum = 0;
  f->upvalues = nullptr;
  f->paramNum = 0;
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

void luaF_freeproto(lua_State *L, Proto *f) {
  luaM_freearray(L, f->code, f->codeSize, Instruction);
  luaM_freearray(L, f->inners, f->pSize, Proto *);
  luaM_freearray(L, f->k, f->kSize, TValue);
  luaM_freearray(L, f->lineInfo, f->lineInfoSize, int);
  luaM_freearray(L, f->locVars, f->locVarsSize, struct LocVar);
  luaM_freearray(L, f->upvalues, f->upvaluesSize, TString *);
  luaM_free(L, f);
}

void luaF_freeclosure(lua_State *L, Closure *c) {
  int size =
      (c->c.isC) ? sizeCclosure(c->c.nupvalues) : sizeLclosure(c->l.nupvalues);
  luaM_freemem(L, c, size);
}

/*
** Look for n-th local variable at line `line` in function `func`.
** Returns nullptr if not found.
*/
const char *luaF_getlocalname(const Proto *f, int local_number, int pc) {
  int i;
  for (i = 0; i < f->locVarsSize && f->locVars[i].startPC <= pc; i++) {
    if (pc < f->locVars[i].endPC) { /* is variable active? */
      local_number--;
      if (local_number == 0) {
        return getstr(f->locVars[i].varname);
      }
    }
  }
  return nullptr; /* not found */
}
