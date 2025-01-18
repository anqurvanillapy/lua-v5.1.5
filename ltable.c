/* Lua tables (hash). */

/*
** Implementation of tables (aka arrays, objects, or hash tables).
** Tables keep its elements in two parts: an array part and a hash part.
** Non-negative integer keys are all candidates to be kept in the array
** part. The actual size of the array is the largest `n' such that at
** least half the slots between 0 and n are in use.
** Hash uses a mix of chained scatter table with Brent's variation.
** A main invariant of these tables is that, if an element is not
** in its main position (i.e. the `original' position that its hash gives
** to it), then the colliding element is in its own main position.
** Hence even when the load factor reaches 100%, performance remains good.
*/

#include <string.h>

#include "lua.h"

#include "ldebug.h"
#include "lgc.h"
#include "lmem.h"
#include "lstate.h"
#include "ltable.h"
#include "object.h"

// Max size of array part is 2^MAXBITS.
#define MAXBITS 26

#define MAXASIZE (1 << MAXBITS)

#define hashpow2(t, n) (gnode(t, lmod((n), sizenode(t))))

#define hashstr(t, str) hashpow2(t, (str)->hash)
#define hashboolean(t, p) hashpow2(t, p)

/*
** for some types, it is better to avoid modulus by power of 2, as
** they tend to have many 2 factors.
*/
#define hashmod(t, n) (gnode(t, ((n) % ((sizenode(t) - 1) | 1))))

#define hashpointer(t, p) hashmod(t, IntPoint(p))

/*
** number of ints inside a lua_Number
*/
#define numints (int)(sizeof(double) / sizeof(int))

#define dummynode (&dummynode_)

static const Node dummynode_ = {
    {{nullptr}, LUA_TYPE_NIL},           /* value */
    {{{nullptr}, LUA_TYPE_NIL, nullptr}} /* key */
};

/*
** hash for lua_Numbers
*/
static Node *hashnum(const Table *t, double n) {
  unsigned int a[numints];
  int i;
  if (luai_numeq(n, 0)) { /* avoid problems with -0 */
    return gnode(t, 0);
  }
  memcpy(a, &n, sizeof(a));
  for (i = 1; i < numints; i++) {
    a[0] += a[i];
  }
  return hashmod(t, a[0]);
}

/*
** returns the `main' position of an element in a table (that is, the index
** of its hash value)
*/
static Node *mainposition(const Table *t, const Value *key) {
  switch (GET_TYPE(key)) {
  case LUA_TYPE_NUMBER:
    return hashnum(t, NUMBER_VALUE(key));
  case LUA_TYPE_STRING:
    return hashstr(t, STRING_VALUE(key));
  case LUA_TYPE_BOOLEAN:
    return hashboolean(t, BOOL_VALUE(key));
  case LUA_TYPE_PTR:
    return hashpointer(t, PTR_VALUE(key));
  default:
    return hashpointer(t, GC_VALUE(key));
  }
}

/*
** returns the index for `key' if `key' is an appropriate key to live in
** the array part of the table, -1 otherwise.
*/
static int arrayindex(const Value *key) {
  if (IS_TYPE_NUMBER(key)) {
    double n = NUMBER_VALUE(key);
    int k;
    lua_number2int(k, n);
    if (luai_numeq((double)k, n)) {
      return k;
    }
  }
  return -1; /* `key' did not match some condition */
}

/*
** returns the index of a `key' for table traversals. First goes all
** elements in the array part, then elements in the hash part. The
** beginning of a traversal is signalled by -1.
*/
static int findindex(lua_State *L, Table *t, StackIndex key) {
  int i;
  if (IS_TYPE_NIL(key)) {
    return -1; /* first iteration */
  }
  i = arrayindex(key);
  if (0 < i && i <= t->sizearray) { /* is `key' inside array part? */
    return i - 1;                   /* yes; that's the index (corrected to C) */
  } else {
    Node *n = mainposition(t, key);
    do { /* check whether `key' is somewhere in the chain */
      /* key may be dead already, but it is ok to use it in `next' */
      if (luaO_rawequalObj(key2tval(n), key) ||
          (GET_TYPE(gkey(n)) == LUA_TYPE_DEAD && IS_COLLECTABLE(key) &&
           GC_VALUE(gkey(n)) == GC_VALUE(key))) {
        i = (int)(n - gnode(t, 0)); /* key index in hash table */
        /* hash elements are numbered after array ones */
        return i + t->sizearray;
      } else {
        n = gnext(n);
      }
    } while (n);
    luaG_runerror(L, "invalid key to " LUA_QUOTE("next")); /* key not found */
    return 0; /* to avoid warnings */
  }
}

int luaH_next(lua_State *L, Table *t, StackIndex key) {
  int i = findindex(L, t, key);       /* find original element */
  for (i++; i < t->sizearray; i++) {  /* try first array part */
    if (!IS_TYPE_NIL(&t->array[i])) { /* a non-nil value? */
      SET_NUMBER(key, (double)(i + 1));
      SET_OBJECT_TO_STACK(L, key + 1, &t->array[i]);
      return 1;
    }
  }
  for (i -= t->sizearray; i < sizenode(t); i++) { /* then hash part */
    if (!IS_TYPE_NIL(gval(gnode(t, i)))) {        /* a non-nil value? */
      SET_OBJECT_TO_STACK(L, key, key2tval(gnode(t, i)));
      SET_OBJECT_TO_STACK(L, key + 1, gval(gnode(t, i)));
      return 1;
    }
  }
  return 0; /* no more elements */
}

/*
** {=============================================================
** Rehash
** ==============================================================
*/

static int computesizes(int nums[], int *narray) {
  int i;
  int twotoi; /* 2^i */
  int a = 0;  /* number of elements smaller than 2^i */
  int na = 0; /* number of elements to go to array part */
  int n = 0;  /* optimal size for array part */
  for (i = 0, twotoi = 1; twotoi / 2 < *narray; i++, twotoi *= 2) {
    if (nums[i] > 0) {
      a += nums[i];
      if (a > twotoi / 2) { /* more than half elements present? */
        n = twotoi;         /* optimal size (till now) */
        na = a; /* all elements smaller than n will go to array part */
      }
    }
    if (a == *narray) {
      break; /* all elements already counted */
    }
  }
  *narray = n;
  DEBUG_ASSERT(*narray / 2 <= na && na <= *narray);
  return na;
}

static int countint(const Value *key, int *nums) {
  int k = arrayindex(key);
  if (0 < k && k <= MAXASIZE) { /* is `key' an appropriate array index? */
    nums[ceillog2(k)]++;        /* count as such */
    return 1;
  } else {
    return 0;
  }
}

static int numusearray(const Table *t, int *nums) {
  int lg;
  int ttlg;     /* 2^lg */
  int ause = 0; /* summation of `nums' */
  int i = 1;    /* count to traverse all array keys */
  for (lg = 0, ttlg = 1; lg <= MAXBITS; lg++, ttlg *= 2) { /* for each slice */
    int lc = 0;                                            /* counter */
    int lim = ttlg;
    if (lim > t->sizearray) {
      lim = t->sizearray; /* adjust upper limit */
      if (i > lim) {
        break; /* no more elements to count */
      }
    }
    /* count elements in range (2^(lg-1), 2^lg] */
    for (; i <= lim; i++) {
      if (!IS_TYPE_NIL(&t->array[i - 1])) {
        lc++;
      }
    }
    nums[lg] += lc;
    ause += lc;
  }
  return ause;
}

static int numusehash(const Table *t, int *nums, int *pnasize) {
  int totaluse = 0; /* total number of elements */
  int ause = 0;     /* summation of `nums' */
  int i = sizenode(t);
  while (i--) {
    Node *n = &t->node[i];
    if (!IS_TYPE_NIL(gval(n))) {
      ause += countint(key2tval(n), nums);
      totaluse++;
    }
  }
  *pnasize += ause;
  return totaluse;
}

static void setarrayvector(lua_State *L, Table *t, int size) {
  int i;
  luaM_reallocvector(L, t->array, t->sizearray, size, Value);
  for (i = t->sizearray; i < size; i++) {
    SET_NIL(&t->array[i]);
  }
  t->sizearray = size;
}

static void setnodevector(lua_State *L, Table *t, int size) {
  int lsize;
  if (size == 0) {                     /* no elements to hash part? */
    t->node = cast(Node *, dummynode); /* use common `dummynode' */
    lsize = 0;
  } else {
    int i;
    lsize = ceillog2(size);
    if (lsize > MAXBITS) {
      luaG_runerror(L, "table overflow");
    }
    size = twoto(lsize);
    t->node = luaM_newvector(L, size, Node);
    for (i = 0; i < size; i++) {
      Node *n = gnode(t, i);
      gnext(n) = nullptr;
      SET_NIL(gkey(n));
      SET_NIL(gval(n));
    }
  }
  t->lsizenode = (uint8_t)lsize;
  t->lastfree = gnode(t, size); /* all positions are free */
}

static void resize(lua_State *L, Table *t, int nasize, int nhsize) {
  int i;
  int oldasize = t->sizearray;
  int oldhsize = t->lsizenode;
  Node *nold = t->node;    /* save old hash ... */
  if (nasize > oldasize) { /* array part must grow? */
    setarrayvector(L, t, nasize);
  }
  /* create new hash part with appropriate size */
  setnodevector(L, t, nhsize);
  if (nasize < oldasize) { /* array part must shrink? */
    t->sizearray = nasize;
    /* re-insert elements from vanishing slice */
    for (i = nasize; i < oldasize; i++) {
      if (!IS_TYPE_NIL(&t->array[i]))
        SET_TABLE_TO_TABLE(L, luaH_setnum(L, t, i + 1), &t->array[i]);
    }
    /* shrink array */
    luaM_reallocvector(L, t->array, oldasize, nasize, Value);
  }
  /* re-insert elements from hash part */
  for (i = twoto(oldhsize) - 1; i >= 0; i--) {
    Node *old = nold + i;
    if (!IS_TYPE_NIL(gval(old)))
      SET_TABLE_TO_TABLE(L, luaH_set(L, t, key2tval(old)), gval(old));
  }
  if (nold != dummynode) {
    luaM_freeArray(L, nold, twoto(oldhsize), Node); /* free old array */
  }
}

void luaH_resizearray(lua_State *L, Table *t, int nasize) {
  int nsize = (t->node == dummynode) ? 0 : sizenode(t);
  resize(L, t, nasize, nsize);
}

static void rehash(lua_State *L, Table *t, const Value *ek) {
  int nasize, na;
  int nums[MAXBITS + 1]; /* nums[i] = number of keys between 2^(i-1) and 2^i */
  int i;
  int totaluse;
  for (i = 0; i <= MAXBITS; i++) {
    nums[i] = 0; /* reset counts */
  }
  nasize = numusearray(t, nums); /* count keys in array part */
  totaluse = nasize;             /* all those keys are integer keys */
  totaluse += numusehash(t, nums, &nasize); /* count keys in hash part */
  /* count extra key */
  nasize += countint(ek, nums);
  totaluse++;
  /* compute new size for array part */
  na = computesizes(nums, &nasize);
  /* resize the table to new computed sizes */
  resize(L, t, nasize, totaluse - na);
}

/*
** }=============================================================
*/

Table *luaH_new(lua_State *L, int narray, int nhash) {
  Table *t = luaM_new(L, Table);
  luaC_link(L, LuaObjectToGCObject(t), LUA_TYPE_TABLE);
  t->metatable = nullptr;
  t->flags = (uint8_t)(~0);
  /* temporary values (kept only if some malloc fails) */
  t->array = nullptr;
  t->sizearray = 0;
  t->lsizenode = 0;
  t->node = cast(Node *, dummynode);
  setarrayvector(L, t, narray);
  setnodevector(L, t, nhash);
  return t;
}

void luaH_free(lua_State *L, Table *t) {
  if (t->node != dummynode) {
    luaM_freeArray(L, t->node, sizenode(t), Node);
  }
  luaM_freeArray(L, t->array, t->sizearray, Value);
  luaM_free(L, t);
}

static Node *getfreepos(Table *t) {
  while (t->lastfree-- > t->node) {
    if (IS_TYPE_NIL(gkey(t->lastfree))) {
      return t->lastfree;
    }
  }
  return NULL; /* could not find a free place */
}

/*
** inserts a new key into a hash table; first, check whether key's main
** position is free. If not, check whether colliding node is in its main
** position or not: if it is not, move colliding node to an empty place and
** put new key in its main position; otherwise (colliding node is in its main
** position), new key goes to an empty position.
*/
static Value *newkey(lua_State *L, Table *t, const Value *key) {
  Node *mp = mainposition(t, key);
  if (!IS_TYPE_NIL(gval(mp)) || mp == dummynode) {
    Node *othern;
    Node *n = getfreepos(t);      /* get a free place */
    if (n == NULL) {              /* cannot find a free place? */
      rehash(L, t, key);          /* grow table */
      return luaH_set(L, t, key); /* re-insert key into grown table */
    }
    DEBUG_ASSERT(n != dummynode);
    othern = mainposition(t, key2tval(mp));
    if (othern != mp) { /* is colliding node out of its main position? */
      /* yes; move colliding node into free position */
      while (gnext(othern) != mp) {
        othern = gnext(othern); /* find previous */
      }
      gnext(othern) = n; /* redo the chain with `n' in place of `mp' */
      *n = *mp; /* copy colliding node into free pos. (mp->next also goes) */
      gnext(mp) = NULL; /* now `mp' is free */
      SET_NIL(gval(mp));
    } else { /* colliding node is in its own main position */
      /* new node will go into free position */
      gnext(n) = gnext(mp); /* chain new position */
      gnext(mp) = n;
      mp = n;
    }
  }
  gkey(mp)->variant = key->variant;
  gkey(mp)->tt = key->tt;
  luaC_barriert(L, t, key);
  DEBUG_ASSERT(IS_TYPE_NIL(gval(mp)));
  return gval(mp);
}

/*
** search function for integers
*/
const Value *luaH_getnum(Table *t, int key) {
  /* (1 <= key && key <= t->sizearray) */
  if (cast(unsigned int, key - 1) < cast(unsigned int, t->sizearray)) {
    return &t->array[key - 1];
  } else {
    double nk = (double)key;
    Node *n = hashnum(t, nk);
    do { /* check whether `key' is somewhere in the chain */
      if (IS_TYPE_NUMBER(gkey(n)) && luai_numeq(NUMBER_VALUE(gkey(n)), nk)) {
        return gval(n); /* that's it */
      } else {
        n = gnext(n);
      }
    } while (n);
    return luaO_nilobject;
  }
}

/*
** search function for strings
*/
const Value *luaH_getstr(Table *t, String *key) {
  Node *n = hashstr(t, key);
  do { /* check whether `key' is somewhere in the chain */
    if (IS_TYPE_STRING(gkey(n)) && STRING_VALUE(gkey(n)) == key) {
      return gval(n); /* that's it */
    } else {
      n = gnext(n);
    }
  } while (n);
  return luaO_nilobject;
}

/*
** main search function
*/
const Value *luaH_get(Table *t, const Value *key) {
  switch (GET_TYPE(key)) {
  case LUA_TYPE_NIL:
    return luaO_nilobject;
  case LUA_TYPE_STRING:
    return luaH_getstr(t, STRING_VALUE(key));
  case LUA_TYPE_NUMBER: {
    int k;
    double n = NUMBER_VALUE(key);
    lua_number2int(k, n);
    if (luai_numeq((double)k, NUMBER_VALUE(key))) { /* index is int? */
      return luaH_getnum(t, k); /* use specialized version */
    }
    /* else go through */
  }
  default: {
    Node *n = mainposition(t, key);
    do { /* check whether `key' is somewhere in the chain */
      if (luaO_rawequalObj(key2tval(n), key)) {
        return gval(n); /* that's it */
      } else {
        n = gnext(n);
      }
    } while (n);
    return luaO_nilobject;
  }
  }
}

Value *luaH_set(lua_State *L, Table *t, const Value *key) {
  const Value *p = luaH_get(t, key);
  t->flags = 0;
  if (p != luaO_nilobject) {
    return cast(Value *, p);
  } else {
    if (IS_TYPE_NIL(key)) {
      luaG_runerror(L, "table index is nil");
    } else if (IS_TYPE_NUMBER(key) && isnan(NUMBER_VALUE(key))) {
      luaG_runerror(L, "table index is NaN");
    }
    return newkey(L, t, key);
  }
}

Value *luaH_setnum(lua_State *L, Table *t, int key) {
  const Value *p = luaH_getnum(t, key);
  if (p != luaO_nilobject) {
    return cast(Value *, p);
  } else {
    Value k;
    SET_NUMBER(&k, (double)key);
    return newkey(L, t, &k);
  }
}

Value *luaH_setstr(lua_State *L, Table *t, String *key) {
  const Value *p = luaH_getstr(t, key);
  if (p != luaO_nilobject) {
    return cast(Value *, p);
  } else {
    Value k;
    SET_STRING(L, &k, key);
    return newkey(L, t, &k);
  }
}

static int unbound_search(Table *t, unsigned int j) {
  unsigned int i = j; /* i is zero or a present index */
  j++;
  /* find `i' and `j' such that i is present and j is not */
  while (!IS_TYPE_NIL(luaH_getnum(t, j))) {
    i = j;
    j *= 2;
    if (j > cast(unsigned int, SAFE_INT_MAX)) { /* overflow? */
      /* table was built with bad purposes: resort to linear search */
      i = 1;
      while (!IS_TYPE_NIL(luaH_getnum(t, i))) {
        i++;
      }
      return i - 1;
    }
  }
  /* now do a binary search between them */
  while (j - i > 1) {
    unsigned int m = (i + j) / 2;
    if (IS_TYPE_NIL(luaH_getnum(t, m))) {
      j = m;
    } else {
      i = m;
    }
  }
  return i;
}

/*
** Try to find a boundary in table `t'. A `boundary' is an integer index
** such that t[i] is non-nil and t[i+1] is nil (and 0 if t[1] is nil).
*/
int luaH_getn(Table *t) {
  unsigned int j = t->sizearray;
  if (j > 0 && IS_TYPE_NIL(&t->array[j - 1])) {
    /* there is a boundary in the array part: (binary) search for it */
    unsigned int i = 0;
    while (j - i > 1) {
      unsigned int m = (i + j) / 2;
      if (IS_TYPE_NIL(&t->array[m - 1])) {
        j = m;
      } else {
        i = m;
      }
    }
    return i;
  }
  /* else must find a boundary in hash part */
  else if (t->node == dummynode) { /* hash part is empty? */
    return j;                      /* that is easy... */
  } else {
    return unbound_search(t, j);
  }
}

#if defined(LUA_DEBUG)

Node *luaH_mainposition(const Table *t, const Value *key) {
  return mainposition(t, key);
}

int luaH_isdummy(Node *n) { return n == dummynode; }

#endif
