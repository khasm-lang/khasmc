#include "khagm_eval.h"
#include "dispatch.h"
#include "khagm_obj.h"
#include "types.h"
extern int khasm_get_argnum;
extern int arity_table(fptr f);

khagm_obj * construct_thunk (khagm_obj * t, khagm_obj ** args, i32 argnum) {
  khagm_obj * new = k_alloc(sizeof(khagm_obj));
  new->jump_point = (fptr) &handle_thunk;
  new->data.callable.args = k_alloc(sizeof(khagm_obj *) * argnum);
  new->data.callable.argnum = argnum;
  memcpy(new->data.callable.args, args, argnum);
  return new;
}

khagm_obj * handle_thunk(khagm_obj * c) {
  i32 argnum = c->data.callable.argnum;
  // blindly throw away type info!
  khagm_obj* (*f)(khagm_obj **) =
    c->data.callable.args[0];
  khagm_obj * ret = f(c->data.callable.args + 1);
  i32 used = ret->used;
  if (argnum == used) {
    return ret;
  }
  else if (argnum > used) {
    // construct thunk
    khagm_obj * thunk = construct_thunk(ret, c->data.callable.args + (argnum - used), (argnum - used));
    return thunk;
  }
  else if (argnum < used) {
    // only case is it didn't use anything, and just gave us back what we started with
    return ret;
  }
  // unreachable
  throw_err("UNREACHABLE", MINOR);
  return NULL;
}

khagm_obj *handle_simpl(khagm_obj *t) { return t; }

khagm_obj *handle_seq(khagm_obj *a) {
  khagm_obj *(*af)(khagm_obj*) =
    a->data.seq.a->jump_point;
  af(a->data.seq.a);
  return a->data.seq.b;
}


void pprint_khagm_obj(khagm_obj * p);
