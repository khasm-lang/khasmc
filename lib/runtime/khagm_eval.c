#include "khagm_eval.h"
#include "dispatch.h"
#include "khagm_obj.h"
#include "types.h"
extern int khasm_get_argnum;
extern int arity_table(fptr);

khagm_obj * reconcile(khagm_obj * ret, khagm_obj ** args,
		      int arity, int argnum) {
  khagm_obj ** offset_args = args + arity;
  i32 offset = argnum - arity;
  switch (ret->type) {
  case call: {
    i32 ret_argnum = ret->data.thunk.argnum;
    ret->data.call.argnum += offset;
    ret->data.call.args = k_realloc(ret->data.call.args,
				  sizeof(khagm_obj*)
				  * ret->data.call.argnum);
    memcpy(ret->data.call.args + ret_argnum,
	   args + arity, offset * sizeof(khagm_obj*));
    return khagm_eval(ret);
  }
  case thunk: {
    i32 ret_argnum = ret->data.thunk.argnum;
    ret->data.thunk.argnum += offset;
    ret->data.thunk.args = k_realloc(ret->data.thunk.args,
				  sizeof(khagm_obj*)
				  * ret->data.thunk.argnum);
    memcpy(ret->data.thunk.args + ret_argnum,
	   args + arity, offset * sizeof(khagm_obj*));
    return khagm_eval(ret);
  }
  case val: {
    khagm_obj * new = k_alloc(sizeof(khagm_obj));
    new->type = call;
    new->data.call.args = k_alloc(sizeof(khagm_obj*) * offset);
    memcpy(new->data.call.args, args + arity, offset * sizeof(khagm_obj*));
    new->data.call.argnum = offset;
    return khagm_eval(new);
  }
  default: {
    throw_err("Cannot eval non-function type\n", FATAL);
    return NULL;
  }
  }
}


int whnf(khagm_obj * root) {
  if (!root) {
    throw_err("whnf null", MINOR);
    return -1;
  }
  switch (root->type) {
  case val:
  case str:
  case ub_float:
  case ub_int:
    return 1;
  default:
    return 0;
  }
  return -1;
}

khagm_obj * khagm_eval_h(khagm_obj * root) {
  if (!root) {
    return NULL;
  }
  switch (root->type) {
  case val:
    printf("val??\n");
  case str:
  case ub_float:
  case ub_int:
    return root;
    break;
  case tuple: {
    for (int i = 0; i < root->data.tuple.num; i++) {
      root->data.tuple.tups[i] =
	khagm_eval(root->data.tuple.tups[i]);
    }
    return root;
    break;
  }
  case call: {
    /*
    for (int i = 0; i < root->data.call.argnum; i++) {
      root->data.call.args[i] =
	khagm_eval(root->data.call.args[i]);
	} */
    int arity = arity_table(root->data.call.function);
    if (arity == -1) {
      throw_err("Invalid function pointer\n", FATAL);
    }
    if (root->data.call.argnum < arity) {
      // not saturated yet
      return root;
    }
    khagm_obj * ret = dispatch(arity,
			       root->data.call.function,
			       root->data.call.args);
    if (root->data.call.argnum > arity) {
      khagm_obj * tmp = reconcile(ret, root->data.call.args,
		       arity, root->data.call.argnum);
      return tmp;
    }
    else {
      k_free(root->data.call.args);
      k_free(root);
      return khagm_eval(ret);
    }
  }
  case thunk: {
    /*
    for (int i = 0; i < root->data.call.argnum; i++) {
      root->data.call.args[i] =
	khagm_eval(root->data.call.args[i]);
	} */
    khagm_obj * ret = khagm_eval(root->data.thunk.function);
    return reconcile(ret,
		     root->data.thunk.args,
		     0,
		     root->data.thunk.argnum);
  }
  case ITE: {
    khagm_obj * ret = khagm_eval(root->data.ITE.ite[0]);
    if (ret->type != ub_int) {
      throw_err("ITE not boolean\n", FATAL);
    }
    if (ret->data.unboxed_int == 1) {
      return khagm_eval(root->data.ITE.ite[1]);
    }
    if (ret->data.unboxed_int == 0) {
      return khagm_eval(root->data.ITE.ite[2]);
    }
    throw_err("ITE not 0/1\n", FATAL);
  }
  case seq: {
    khagm_eval(root->data.seq.a);
    return khagm_eval(root->data.seq.b);
  }
  default:
    printf("ERROR TYPE: %d\n", root->type);
    throw_err("UNREACHABLE: khagm_eval", MAJOR);
    printf("OBJECT LOOKS LIKE:\n");
    pprint_khagm_obj(root);
    for (int i = 0; i < 20; i++) {
      printf("%2X\n", (char)root->data.FULL[i]);
    }
    return NULL;
  }
}

khagm_obj * khagm_eval(khagm_obj * root) {
  int iter = 0;
  if (!whnf(root)) {
    iter++;
    root = khagm_eval_h(root);
  }
  if (iter > 1)
    printf("iters: %d\n", iter);
  return root;
}

void pprint_khagm_obj(khagm_obj * p);
