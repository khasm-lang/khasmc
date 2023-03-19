#include "err.h"
#include "khagm_obj.h"
#include "khagm_alloc.h"
#include "create.h"

#define new_kobj(nm) khagm_obj * nm = k_alloc(sizeof(khagm_obj))
khagm_obj * extern_1614757695_int1597980479_add(khagm_obj * a, khagm_obj * b) {
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int + b->data.unboxed_int;
    k_free(a);
    k_free(b);
    return k;
  }
  throw_err("attempted to add non ints", FATAL);
  return NULL;
}

khagm_obj *extern_1614757695_int1597980479_sub
    (khagm_obj * a, khagm_obj * b) {
  printf("sub %p %p\n", a, b);
  pprint_khagm_obj(a);
  pprint_khagm_obj(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int - b->data.unboxed_int;
    k_free(a);
    k_free(b);
    return k;
  }
  throw_err("attempted to sub non ints", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_int1597980479_mul(khagm_obj * a, khagm_obj * b) {
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int * b->data.unboxed_int;
    k_free(a);
    k_free(b);
    return k;
  }
  throw_err("attempted to mul non ints", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_int1597980479_div(khagm_obj * a, khagm_obj * b) {
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = (i64) a->data.unboxed_int / b->data.unboxed_int;
    k_free(a);
    k_free(b);
    return k;
  }
  throw_err("attempted to div non ints", FATAL);
  return NULL;
}


khagm_obj * extern_1614757695_float1597980479_add(khagm_obj * a, khagm_obj * b) {
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float + b->data.unboxed_float;
    k_free(a);
    k_free(b);
    return k;
  }
  throw_err("attempted to add non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_sub(khagm_obj * a, khagm_obj * b) {
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float - b->data.unboxed_float;
    k_free(a);
    k_free(b);
    return k;
  }
  throw_err("attempted to sub non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_mul(khagm_obj * a, khagm_obj * b) {
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float * b->data.unboxed_float;
    k_free(a);
    k_free(b);
    return k;
  }
  throw_err("attempted to mul non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_div(khagm_obj * a, khagm_obj * b) {
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = (f64) a->data.unboxed_float / b->data.unboxed_float;
    k_free(a);
    k_free(b);
    return k;
  }
  throw_err("attempted to div non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_print(khagm_obj * a) {
  pprint_khagm_obj(a);
  return create_tuple(create_list(0, NULL),0);
}


khagm_obj *extern_1614757695_s1597980479_eq
    (khagm_obj * a, khagm_obj * b) {
  if (a->type == b->type && a->data.FULL == b->data.FULL) {
    free(a);
    free(b);
    return create_int(1);
  }
  else {
    free(a);
    free(b);
    return create_int(0);
  }
}
