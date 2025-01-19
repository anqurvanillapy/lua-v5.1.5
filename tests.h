/* Internal Header for Debugging of the Lua Implementation. */

#pragma once

#define LUA_INTERNAL_TESTING

#include <stdlib.h>

#ifdef NDBUG
#error "unexpected internal testing on release mode"
#endif

/* memory allocator control variables */
typedef struct Memcontrol {
  unsigned long numblocks;
  unsigned long total;
  unsigned long maxmem;
  unsigned long memlimit;
} Memcontrol;

extern Memcontrol memcontrol;

/*
** generic variable for debug tricks
*/
extern int Trick;

void *debug_realloc(void *ud, void *block, size_t osize, size_t nsize);

typedef struct CallInfo *pCallInfo;

typedef struct lua_State lua_State;
int lua_checkmemory(lua_State *L);
int lua_checkpc(lua_State *L, pCallInfo ci);

/* test for lock/unlock */

struct L_EXTRA {
  int lock;
  int *plock;
};

#undef LUAI_EXTRASPACE
#define LUAI_EXTRASPACE sizeof(struct L_EXTRA)

#undef luai_userstateopen
#undef luai_userstatethread
#define luai_userstateopen(l)                                                  \
  (getlock(l)->lock = 0, getlock(l)->plock = &(getlock(l)->lock))
#define luai_userstatethread(l, l1) (getlock(l1)->plock = getlock(l)->plock)
#undef lua_lock
#undef lua_unlock
#define lua_lock(l) assert((*getlock(l)->plock)++ == 0)
#define lua_unlock(l) assert(--(*getlock(l)->plock) == 0)

#define getlock(l) (cast(struct L_EXTRA *, l) - 1)

int luaB_opentests(lua_State *L);

/* real main will be defined at `ltests.c' */
int l_main(int argc, const char *argv[]);
#define main l_main

/* change some sizes to give some bugs a chance */

#undef LUAL_BUFFER_SIZE
#define LUAL_BUFFER_SIZE 27
#define MINSTRTABSIZE 2
