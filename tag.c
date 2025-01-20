#include <string.h>

#include "intern.h"
#include "object.h"
#include "state.h"
#include "table.h"
#include "tag.h"

void luaT_init(lua_State *L) {
  static const char *const events[] = {
      "__index", "__newindex", "__gc",  "__mode",   "__eq",   "__add",
      "__sub",   "__mul",      "__div", "__mod",    "__pow",  "__unm",
      "__len",   "__lt",       "__le",  "__concat", "__call",
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
