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
** Hence, even when the load factor reaches 100%, performance remains good.
*/

#include <string.h>

#include "debug.h"
#include "gc.h"
#include "memory.h"
#include "object.h"
#include "state.h"
#include "table.h"

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

static const Node dummy = {
    .i_val = {{nullptr}, LUA_TYPE_NIL},
    .i_key = {{{nullptr}, LUA_TYPE_NIL, nullptr}},
};

/*
** hash for lua_Numbers
*/
static Node *hashnum(const Table *t, double n) {
  if (n == 0) {
    return gnode(t, 0);
  }
  unsigned int a[numints];
  memcpy(a, &n, sizeof(a));
  for (int i = 1; i < numints; i++) {
    a[0] += a[i];
  }
  return hashmod(t, a[0]);
}

static Node *getBucket(const Table *t, const Value *key) {
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
** returns the index for 'key' if 'key' is an appropriate key to live in
** the array part of the table, -1 otherwise.
*/
static int arrayIndex(const Value *key) {
  if (IS_TYPE_NUMBER(key)) {
    double n = NUMBER_VALUE(key);
    int k;
    lua_number2int(k, n);
    if ((double)k == n) {
      return k;
    }
  }
  return -1;
}

/*
** returns the index of a `key' for table traversals. First goes all
** elements in the array part, then elements in the hash part. The
** beginning of a traversal is signalled by -1.
*/
static int findindex(lua_State *L, Table *t, StackIndex key) {
  if (IS_TYPE_NIL(key)) {
    return -1; /* first iteration */
  }
  int i = arrayIndex(key);
  if (0 < i && i <= t->sizearray) { /* is `key' inside array part? */
    return i - 1;                   /* yes; that's the index (corrected to C) */
  }
  Node *n = getBucket(t, key);
  do {
    /* key may be dead already, but it is ok to use it in `next' */
    if (luaO_rawequalObj(key2tval(n), key) ||
        (GET_TYPE(gkey(n)) == LUA_TYPE_DEAD && IS_COLLECTABLE(key) &&
         GC_VALUE(gkey(n)) == GC_VALUE(key))) {
      i = (int)(n - gnode(t, 0)); /* key index in hash table */
      /* hash elements are numbered after array ones */
      return i + t->sizearray;
    }
    n = gnext(n);
  } while (n);
  luaG_runerror(L, "invalid key to 'next'"); /* key not found */
  return 0;
}

int Table_next(lua_State *L, Table *t, StackIndex key) {
  int i = findindex(L, t, key) + 1;   /* find original element */
  for (; i < t->sizearray; i++) {     /* try first array part */
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

static int computesizes(const int nums[], int *narray) {
  int i = 0;
  int twoToI = 1; /* 2^i */
  int a = 0;      /* number of elements smaller than 2^i */
  int na = 0;     /* number of elements to go to array part */
  int n = 0;      /* optimal size for array part */
  for (; twoToI / 2 < *narray; i++, twoToI *= 2) {
    if (nums[i] > 0) {
      a += nums[i];
      if (a > twoToI / 2) { /* more than half elements present? */
        n = twoToI;         /* optimal size (till now) */
        na = a; /* all elements smaller than n will go to array part */
      }
    }
    if (a == *narray) {
      break; /* all elements already counted */
    }
  }
  *narray = n;
  assert(*narray / 2 <= na && na <= *narray);
  return na;
}

static int countint(const Value *key, int *nums) {
  int k = arrayIndex(key);
  if (0 < k && k <= MAXASIZE) { /* is `key' an appropriate array index? */
    nums[ceillog2(k)]++;        /* count as such */
    return 1;
  }
  return 0;
}

static int numusearray(const Table *t, int *nums) {
  int lg = 0;
  int twoToLg = 1; /* 2^lg */
  int numsTotal = 0;
  int i = 1; /* count to traverse all array keys */
  for (; lg <= MAXBITS; lg++, twoToLg *= 2) { /* for each slice */
    int lc = 0;                               /* counter */
    int lim = twoToLg;
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
    numsTotal += lc;
  }
  return numsTotal;
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

static void resizeArrayVector(lua_State *L, Table *t, int size) {
  Mem_reallocVec(L, t->array, t->sizearray, size, Value);
  for (int i = t->sizearray; i < size; i++) {
    SET_NIL(&t->array[i]);
  }
  t->sizearray = size;
}

static void resizeBuckets(lua_State *L, Table *t, int size) {
  int lsize = 0;
  if (size == 0) { /* no elements to hash part? */
    t->node = (Node *)&dummy;
  } else {
    lsize = ceillog2(size);
    if (lsize > MAXBITS) {
      luaG_runerror(L, "table overflow");
    }
    size = twoto(lsize);
    t->node = Mem_newVec(L, size, Node);
    for (int i = 0; i < size; i++) {
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
  int oldasize = t->sizearray;
  int oldhsize = t->lsizenode;
  Node *nold = t->node;    /* save old hash ... */
  if (nasize > oldasize) { /* array part must grow? */
    resizeArrayVector(L, t, nasize);
  }
  /* create new hash part with appropriate size */
  resizeBuckets(L, t, nhsize);
  if (nasize < oldasize) { /* array part must shrink? */
    t->sizearray = nasize;
    /* re-insert elements from vanishing slice */
    for (int i = nasize; i < oldasize; i++) {
      if (!IS_TYPE_NIL(&t->array[i]))
        SET_TABLE_TO_TABLE(L, Table_insertInteger(L, t, i + 1), &t->array[i]);
    }
    /* shrink array */
    Mem_reallocVec(L, t->array, oldasize, nasize, Value);
  }
  /* re-insert elements from hash part */
  for (int i = twoto(oldhsize) - 1; i >= 0; i--) {
    Node *old = nold + i;
    if (!IS_TYPE_NIL(gval(old)))
      SET_TABLE_TO_TABLE(L, Table_insert(L, t, key2tval(old)), gval(old));
  }
  if (nold != &dummy) {
    Mem_freeVec(L, nold, twoto(oldhsize), Node); /* free old array */
  }
}

void Table_resizeArray(lua_State *L, Table *t, int nasize) {
  int nsize = t->node == &dummy ? 0 : sizenode(t);
  resize(L, t, nasize, nsize);
}

static void rehash(lua_State *L, Table *t, const Value *ek) {
  int nums[MAXBITS + 1]; /* nums[i] = number of keys between 2^(i-1) and 2^i */
  for (int i = 0; i <= MAXBITS; i++) {
    nums[i] = 0; /* reset counts */
  }
  int nasize = numusearray(t, nums); /* count keys in array part */
  int totaluse = nasize;             /* all those keys are integer keys */
  totaluse += numusehash(t, nums, &nasize); /* count keys in hash part */
  /* count extra key */
  nasize += countint(ek, nums);
  totaluse++;
  /* compute new size for array part */
  int na = computesizes(nums, &nasize);
  /* resize the table to new computed sizes */
  resize(L, t, nasize, totaluse - na);
}

Table *Table_new(lua_State *L, int narray, int nhash) {
  Table *t = Mem_new(L, Table);
  luaC_link(L, LuaObjectToGCObject(t), LUA_TYPE_TABLE);
  t->metatable = nullptr;
  t->flags = (uint8_t)(~0);
  /* temporary values (kept only if some malloc fails) */
  t->array = nullptr;
  t->sizearray = 0;
  t->lsizenode = 0;
  t->node = (Node *)&dummy;
  resizeArrayVector(L, t, narray);
  resizeBuckets(L, t, nhash);
  return t;
}

void Table_free(lua_State *L, Table *t) {
  if (t->node != &dummy) {
    Mem_freeVec(L, t->node, sizenode(t), Node);
  }
  Mem_freeVec(L, t->array, t->sizearray, Value);
  Mem_freePtr(L, t);
}

static Node *getFreePos(Table *t) {
  while (t->lastfree-- > t->node) {
    if (IS_TYPE_NIL(gkey(t->lastfree))) {
      return t->lastfree;
    }
  }
  return nullptr;
}

/*
** inserts a new key into a hash table; first, check whether key's main
** position is free. If not, check whether colliding node is in its main
** position or not: if it is not, move colliding node to an empty place and
** put new key in its main position; otherwise (colliding node is in its main
** position), new key goes to an empty position.
*/
static Value *insertNewKey(lua_State *L, Table *t, const Value *key) {
  Node *mp = getBucket(t, key);
  if (!IS_TYPE_NIL(gval(mp)) || mp == &dummy) {
    Node *n = getFreePos(t);          /* get a free place */
    if (n == nullptr) {               /* cannot find a free place? */
      rehash(L, t, key);              /* grow table */
      return Table_insert(L, t, key); /* re-insert key into grown table */
    }
    assert(n != &dummy);
    Node *other = getBucket(t, key2tval(mp));
    if (other != mp) { /* is colliding node out of its main position? */
      /* yes; move colliding node into free position */
      while (gnext(other) != mp) {
        other = gnext(other); /* find previous */
      }
      gnext(other) = n; /* redo the chain with `n' in place of `mp' */
      *n = *mp; /* copy colliding node into free pos. (mp->next also goes) */
      gnext(mp) = nullptr; /* now `mp' is free */
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
  assert(IS_TYPE_NIL(gval(mp)));
  return gval(mp);
}

const Value *Table_getInteger(Table *t, int key) {
  // 1 <= key <= array size.
  if ((size_t)(key - 1) < (size_t)t->sizearray) {
    return &t->array[key - 1];
  }
  double nk = (double)key;
  Node *n = hashnum(t, nk);
  do {
    if (IS_TYPE_NUMBER(gkey(n)) && NUMBER_VALUE(gkey(n)) == nk) {
      return gval(n);
    }
    n = gnext(n);
  } while (n);
  return &valueNil;
}

const Value *Table_getString(Table *t, String *key) {
  Node *n = hashstr(t, key);
  do { /* check whether `key' is somewhere in the chain */
    if (IS_TYPE_STRING(gkey(n)) && STRING_VALUE(gkey(n)) == key) {
      return gval(n); /* that's it */
    }
    n = gnext(n);
  } while (n);
  return &valueNil;
}

const Value *Table_get(Table *t, const Value *key) {
  switch (GET_TYPE(key)) {
  case LUA_TYPE_NIL:
    return &valueNil;
  case LUA_TYPE_STRING:
    return Table_getString(t, STRING_VALUE(key));
  case LUA_TYPE_NUMBER: {
    int k;
    double n = NUMBER_VALUE(key);
    lua_number2int(k, n);
    if ((double)k == NUMBER_VALUE(key)) { /* index is int? */
      return Table_getInteger(t, k);      /* use specialized version */
    }
    [[fallthrough]];
  }
  default: {
    Node *n = getBucket(t, key);
    do { /* check whether 'key' is somewhere in the chain */
      if (luaO_rawequalObj(key2tval(n), key)) {
        return gval(n); /* that's it */
      }
      n = gnext(n);
    } while (n);
    return &valueNil;
  }
  }
}

Value *Table_insert(lua_State *L, Table *t, const Value *key) {
  const Value *p = Table_get(t, key);
  t->flags = 0;
  if (p != &valueNil) {
    return (Value *)p;
  }
  if (IS_TYPE_NIL(key)) {
    luaG_runerror(L, "table index is nil");
  } else if (IS_TYPE_NUMBER(key) && isnan(NUMBER_VALUE(key))) {
    luaG_runerror(L, "table index is NaN");
  }
  return insertNewKey(L, t, key);
}

Value *Table_insertInteger(lua_State *L, Table *t, int key) {
  const Value *p = Table_getInteger(t, key);
  if (p != &valueNil) {
    return (Value *)p;
  }
  Value k;
  SET_NUMBER(&k, (double)key);
  return insertNewKey(L, t, &k);
}

Value *Table_insertString(lua_State *L, Table *t, String *key) {
  const Value *p = Table_getString(t, key);
  if (p != &valueNil) {
    return (Value *)p;
  }
  Value k;
  SET_STRING(L, &k, key);
  return insertNewKey(L, t, &k);
}

static int unboundSearch(Table *t, unsigned int j) {
  unsigned int i = j; /* i is zero or a present index */
  j++;
  /* find `i' and `j' such that i is present and j is not */
  while (!IS_TYPE_NIL(Table_getInteger(t, j))) {
    i = j;
    j *= 2;
    if (j > SAFE_INT_MAX) { /* overflow? */
      /* table was built with bad purposes: resort to linear search */
      i = 1;
      while (!IS_TYPE_NIL(Table_getInteger(t, i))) {
        i++;
      }
      return i - 1;
    }
  }
  /* now do a binary search between them */
  while (j - i > 1) {
    unsigned int m = (i + j) / 2;
    if (IS_TYPE_NIL(Table_getInteger(t, m))) {
      j = m;
    } else {
      i = m;
    }
  }
  return i;
}

/// Try to find a boundary in table `t`. A boundary is an integer index such
/// that `t[i]` is non-nil and `t[i+1]` is nil (and `0` if `t[1]` is nil).
int Table_getBoundary(Table *t) {
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
  if (t->node == &dummy) { /* hash part is empty? */
    return j;
  }
  /* else must find a boundary in hash part */
  return unboundSearch(t, j);
}

#ifdef LUA_INTERNAL_TESTING
Node *Table_internalGetBucket(const Table *t, const Value *key) {
  return getBucket(t, key);
}

bool Table_internalIsDummy(Node *n) { return n == &dummy; }
#endif
