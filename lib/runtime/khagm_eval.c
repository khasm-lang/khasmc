#include "khagm_eval.h"
#include "dispatch.h"
#include "err.h"
#include "khagm_obj.h"
#include "types.h"
#include <stdio.h>
extern int khasm_get_argnum;
extern int arity_table(fptr f);
enum SAT_STATES {
  NOT_SAT = -10,
  INVALID,
};

/*
  BEWARE
  Because of the choice to have args[0] be the pointer to the
  inner callable, this whole thing is rife with off-by-one errors.
  be careful.
*/

khagm_obj * handle_overflow(khagm_obj * ret,
			    khagm_obj * orig,
			    i32 argnum) {
  i32 used = get_used(ret);
  printf("handle %d %d\n", argnum, used);
  if (used == argnum) {
    // we've used all the args
    return ret;
  }
  else if (used < argnum) {
    // construct new thunk
    khagm_obj * new = k_alloc(sizeof(khagm_obj));
    new->jump_point = &handle_thunk;
    new->data.callable.argnum = (argnum - used);
    new->data.callable.args =
      k_alloc(sizeof(khagm_obj *)
	      * ((argnum - used) + 1));
    new->data.callable.args[0] = ret;
    memcpy(new->data.callable.args + 1,
	   orig->data.callable.args + used + 1,
	   argnum - used);
    return set_used(new, INVALID);
  }
  else if (used > argnum) {
    // impossible
    throw_err("IMPOSSIBLE: more used then args", FATAL);
  }
  throw_err("UNREACHABLE", FATAL);
  return NULL;
}



khagm_obj * handle_thunk(khagm_obj * c) {
  i32 argnum = c->data.callable.argnum;
  printf("handle thunk %p %d\n", c, argnum);
  khagm_obj * ret =
    khagm_whnf(c->data.callable.args[0]);
  i32 used = get_used(ret);
  if (used == -1) {
    printf("fptr\n");
    // fptr
    khagm_obj *(*f)(khagm_obj**, i32)
      = c->data.callable.args[0]->data.val;
    khagm_obj * t = f(c->data.callable.args + 1, argnum);
    if (!t) {
      // args not saturated
      return set_used(c, NOT_SAT);
    }
    // args saturated, handle next bit
    return handle_overflow(t, c, argnum);
  }
  else {
    printf("thunky\n");
    // we know it's a thunk in there
    if (used == NOT_SAT) {
      // not enough arguments, so we need to add ours
      i32 sum = c->data.callable.argnum
	+ ret->data.callable.argnum;
      ret->data.callable.args =
	k_realloc(ret->data.callable.args,
		  sizeof(khagm_obj*) * (sum + 1));
      memcpy(ret->data.callable.args +
	     ret->data.callable.argnum + 1,
	     c->data.callable.args + 1,
	     c->data.callable.argnum);
      ret->data.callable.argnum = sum;
      set_used(ret, INVALID);
      return khagm_whnf(ret);
    }
    else if (used < argnum) {
      // construct a new thunky wunky
      khagm_obj * new = k_alloc(sizeof(khagm_obj));
      new->jump_point = &handle_thunk;
      new->data.callable.args = k_alloc(sizeof(khagm_obj *)
			  * ((argnum - used) + 1));
      new->data.callable.argnum = argnum - used;
      memcpy(new->data.callable.args,
	     ret->data.callable.args + used,
	     argnum - used);
      return set_used(new, INVALID);
    }
    else {
      throw_err("UNREACHABLE : used > argnum", FATAL);
      return NULL;
    }
  }
  // unreachable
  throw_err("UNREACHABLE", MINOR);
  return NULL;
}

khagm_obj *handle_simpl(khagm_obj *t) { return set_used(t, -1); }

khagm_obj *handle_seq(khagm_obj *a) {
  khagm_whnf(a->data.seq.a[0]);
  return a->data.seq.a[1];
}


void pprint_khagm_obj(khagm_obj * p) {
  printf("NOTIMPL : print\n");
}

khagm_obj * khagm_whnf(khagm_obj * a) {
  // TODO : this is not true weak head normal form, because reasons
  set_used(a, -4);
  u64 oldb, newb;
  u32 olds, news;
  while (1) {
    oldb = *a->data.FULL;
    olds = *(a->data.FULL + 8);
    if (get_used(a) == -1) {
      break;
    }
    a = khagm_eval(a);
    newb = *a->data.FULL;
    news = *(a->data.FULL + 8);
    if (oldb == newb && olds == news) {
      break;
    }
    printf("WHNF: %d %d\n", oldb == newb, olds==news);
  }
  return a;
}
