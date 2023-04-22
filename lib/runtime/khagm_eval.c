#include "khagm_eval.h"
#include "dispatch.h"
#include "err.h"
#include "khagm_obj.h"
#include "types.h"
#include <stdio.h>
extern int khasm_get_argnum;
extern int arity_table(fptr f);
#define UNREACHABLE __builtin_unreachable();\
  throw_err("UNREACHABLE", FATAL);\
  return NULL;

/*
  BEWARE
  Because of the choice to have args[0] be the pointer to the
  inner callable, this whole thing is rife with off-by-one errors.
  be careful.
*/

enum Saturated {
  UNSAT = -100,
  SAT,
  SIMPL_VAL,
};

khagm_obj * handle_thunk(khagm_obj * c) {
  khagm_obj * first = khagm_whnf(c->data.callable.args[0]);
  if (get_used(first) == SIMPL_VAL) {
    khagm_obj *(*f)(khagm_obj **, i32)
      =  (khagm_obj *(*)(khagm_obj **, i32)) first->data.val;
    khagm_obj * ret = f(c->data.callable.args + 1,
			c->data.callable.argnum);
    if (!ret) {
      // not enough args yet
      return set_used(c, UNSAT);
    }
    else {
      if (ret->used == c->data.callable.argnum) {
	memmove(c, ret, sizeof(khagm_obj));
	return c;
      }
      // enough args and we just got a new thing, so construct a
      i32 diff = c->data.callable.argnum - ret->used;
      // new thunk
      khagm_obj * new = k_alloc(sizeof(khagm_obj));
      new->jump_point = &handle_thunk;
      new->data.callable.args = k_alloc(sizeof(khagm_obj*)
					* (diff + 1));
      memmove(new->data.callable.args + 1,
	      c->data.callable.args + 1,
	      sizeof(khagm_obj*) * diff);
      new->data.callable.args[0] = ret;
      new->data.callable.argnum = diff;
      memmove(c, new, sizeof(khagm_obj));
      return set_used(khagm_eval(c), get_used(c) + 1);
    }
  }
  else if (get_used(first) == UNSAT){
    // unsat thunk
    // copy
    khagm_obj * copy = khagm_obj_copy_thunk(first);
    // add our args
    i32 sum = copy->data.callable.argnum
      + c->data.callable.argnum;
    copy->data.callable.args =
      k_realloc(copy->data.callable.args,
		/* account for 1 indexing */
		sizeof(khagm_obj *) * (sum + 1));
    /* again account for 1 indexing */
    memmove(copy->data.callable.args + copy->data.callable.argnum + 1,
	   c->data.callable.args + 1,
	    sizeof(khagm_obj *) * c->data.callable.argnum);
    copy->data.callable.argnum = sum;
    memmove(c, copy, sizeof(khagm_obj));
    return set_used(c, get_used(c) + 1);
  }
  else {
    // sat thunk
    c->data.callable.args[0] = first;
    i32 tmp = get_used(c);
    khagm_obj * tmp2 = khagm_eval(c);
    return set_used(tmp2, tmp+1);
  }
  UNREACHABLE;
}

khagm_obj *handle_simpl(khagm_obj *t) { return set_used(t, SIMPL_VAL); }

khagm_obj *handle_seq(khagm_obj *a) {
  khagm_whnf(a->data.seq.a[0]);
  return a->data.seq.a[1];
}


void pprint_khagm_obj(khagm_obj * p) {
  printf("NOTIMPL : print\n");
}

khagm_obj * khagm_whnf(khagm_obj * a) {
  // TODO : this is not true weak head normal form, because reasons
  u64 oldb, newb;
  u32 olds, news;
  i32 oused, nused;
  while (1) {
    oldb = *a->data.FULL;
    olds = *(a->data.FULL + 8);
    oused = a->used;
    if (get_used(a) == SIMPL_VAL) {
      break;
    }
    else if (get_used(a) == UNSAT) {
      break;
    }
    a = khagm_eval(a);
    newb = *a->data.FULL;
    news = *(a->data.FULL + 8);
    nused = a->used;
    if (oldb == newb && olds == news && oused == nused) {
      break;
    }
  }
  return a;
}
