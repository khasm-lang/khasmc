#include "err.h"
#include "khagm_eval.h"
#include "khagm_obj.h"
#include "khagm_alloc.h"
#include "create.h"

#define new_kobj(nm) khagm_obj * nm = k_alloc(sizeof(khagm_obj))
inline khagm_obj * extern_1614757695_int1597980479_add(khagm_obj**args, i32 c) {
  if (c < 2) {
    return NULL;
  }
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.unboxed_int =
    khagm_whnf(args[0])->data.unboxed_int
    +
    khagm_whnf(args[1])->data.unboxed_int;
  return set_used(k, 2);
}
khagm_obj *extern_1614757695_int1597980479_sub(khagm_obj **args, i32 c)  {
  if (c < 2) {
    return NULL;
  }
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.unboxed_int =
    khagm_whnf(args[0])->data.unboxed_int
    -
    khagm_whnf(args[1])->data.unboxed_int;
  return set_used(k, 2);
}
khagm_obj *extern_1614757695_int1597980479_mul(khagm_obj **args, i32 c)  {
  if (c < 2) {
    return NULL;
  }
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.unboxed_int =
    khagm_whnf(args[0])->data.unboxed_int
    *
    khagm_whnf(args[1])->data.unboxed_int;
  return set_used(k, 2);
}
khagm_obj *extern_1614757695_int1597980479_div(khagm_obj **args, i32 c)  {
  if (c < 2) {
    return NULL;
  }
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.unboxed_int =
    (i64)
    khagm_whnf(args[0])->data.unboxed_int
    /
    khagm_whnf(args[1])->data.unboxed_int;
  return set_used(k, 2);
}
khagm_obj *extern_1614757695_float1597980479_add(khagm_obj **args, i32 c);
khagm_obj *extern_1614757695_float1597980479_sub(khagm_obj **args, i32 c);
khagm_obj *extern_1614757695_float1597980479_mul(khagm_obj **args, i32 c);
khagm_obj *extern_1614757695_float1597980479_div(khagm_obj **args, i32 c);

khagm_obj * extern_1614757695_print1597980479_int(khagm_obj ** a, i32 c) {
  if (c < 1) {
    return NULL;
  }
  khagm_obj * b = khagm_whnf(a[0]);
  printf("awoga %ld\n", b->data.unboxed_int);
  khagm_obj * tmp = create_tuple(create_list(0, NULL),0);
  set_used(tmp, 1);
  return tmp;
}
khagm_obj * extern_1614757695_debug(khagm_obj ** a, i32 c) {
  if (c < 1) {
    return NULL;
  }
  printf("DEBUG\n");
  return set_used(create_tuple(create_list(0, NULL), 0), 1);
}


inline khagm_obj *extern_1614757695_s1597980479_eq
(khagm_obj **a, i32 c) {
  if (c < 2) {
    return NULL;
  }
  if (khagm_obj_eq(khagm_whnf(a[0]), khagm_whnf(a[1]))) {
    return set_used(create_int(1), 2);
  }
  return set_used(create_int(0), 2);
}
