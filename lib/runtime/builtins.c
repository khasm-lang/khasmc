#include "err.h"
#include "khagm_eval.h"
#include "khagm_obj.h"
#include "khagm_alloc.h"
#include "create.h"

#define new_kobj(nm) khagm_obj * nm = k_alloc(sizeof(khagm_obj))
khagm_obj * extern_1614757695_int1597980479_add(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int + b->data.unboxed_int;
    return k;
  }
  throw_err("attempted to add non ints", FATAL);
  return NULL;
}

khagm_obj *extern_1614757695_int1597980479_sub
    (khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int - b->data.unboxed_int;
    return k;
  }
  throw_err("attempted to sub non ints", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_int1597980479_mul(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int * b->data.unboxed_int;
    return k;
  }
  throw_err("attempted to mul non ints", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_int1597980479_div(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = (i64) a->data.unboxed_int / b->data.unboxed_int;
    return k;
  }
  throw_err("attempted to div non ints", FATAL);
  return NULL;
}


khagm_obj * extern_1614757695_float1597980479_add(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float + b->data.unboxed_float;
    return k;
  }
  throw_err("attempted to add non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_sub(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float - b->data.unboxed_float;
    return k;
  }
  throw_err("attempted to sub non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_mul(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float * b->data.unboxed_float;
    return k;
  }
  throw_err("attempted to mul non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_div(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = (f64) a->data.unboxed_float / b->data.unboxed_float;
    return k;
  }
  throw_err("attempted to div non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_debug(khagm_obj * a) {
  khagm_obj * b = khagm_eval(a);
  pprint_khagm_obj(b);
  return create_tuple(create_list(0, NULL),0);
}


khagm_obj *extern_1614757695_s1597980479_eq
    (khagm_obj * a, khagm_obj * b) {
  if (khagm_obj_eq(a, b)) {
    return create_int(1);
  }
  return create_int(0);
}

khagm_obj *extern_1614757695_force(khagm_obj * a) {
  return khagm_eval(a);
}
