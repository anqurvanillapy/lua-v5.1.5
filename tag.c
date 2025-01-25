#include "tag.h"
#include "intern.h"
#include "object.h"
#include "state.h"
#include "table.h"

void luaT_init(lua_State *L) {
  static const char *const events[] = {
      [TM_INDEX] = "__index", [TM_NEWINDEX] = "__newindex",
      [TM_GC] = "__gc",       [TM_MODE] = "__mode",
      [TM_EQ] = "__eq",       [TM_ADD] = "__add",
      [TM_SUB] = "__sub",     [TM_MUL] = "__mul",
      [TM_DIV] = "__div",     [TM_MOD] = "__mod",
      [TM_POW] = "__pow",     [TM_UNM] = "__unm",
      [TM_LEN] = "__len",     [TM_LT] = "__lt",
      [TM_LE] = "__le",       [TM_CONCAT] = "__concat",
      [TM_CALL] = "__call",
  };
  for (size_t i = 0; i < TM_N; i++) {
    G(L)->tmname[i] = String_create(L, events[i]);
    String_intern(G(L)->tmname[i]);
  }
}

/*
** function to be used with macro "FAST_TM": optimized for absence of
** tag methods
*/
const Value *luaT_gettm(Table *events, TMS event, String *ename) {
  const Value *tm = Table_getString(events, ename);
  assert(event <= TM_EQ);
  if (IS_TYPE_NIL(tm)) {                     /* no tag method? */
    events->flags |= (uint8_t)(1u << event); /* cache this fact */
    return nullptr;
  }
  return tm;
}

const Value *luaT_gettmbyobj(lua_State *L, const Value *o, TMS event) {
  Table *mt;
  switch (GET_TYPE(o)) {
  case LUA_TYPE_TABLE:
    mt = TABLE_VALUE(o)->metatable;
    break;
  case LUA_TYPE_USERDATA:
    mt = USERDATA_VALUE(o)->metatable;
    break;
  default:
    mt = G(L)->mt[GET_TYPE(o)];
  }
  return mt ? Table_getString(mt, G(L)->tmname[event]) : &valueNil;
}
