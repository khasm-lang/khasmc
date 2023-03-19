#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include "khagm_alloc.h"
#include "khagm_obj.h"

#define new_kobj(nm) khagm_obj * nm = k_alloc(sizeof(khagm_obj)) 

khagm_obj * create_val(fptr f) {
  new_kobj(k);
  k->type = val;
  k->data.val = f;
  return k;
}

khagm_obj *create_call
    (fptr f, khagm_obj ** args, i32 argnum) {
  new_kobj(k);
  k->type = call;
  k->data.call.function = f;
  k->data.call.args = args;
  k->data.call.argnum = argnum;
  return k;
}

khagm_obj *create_thunk
    (khagm_obj * function, khagm_obj ** args, i32 argnum) {
  new_kobj(k);
  k->type = thunk;
  k->data.thunk.function = function;
  k->data.thunk.args = args;
  k->data.thunk.argnum = argnum;
  return k;
}

khagm_obj *create_tuple(khagm_obj ** tups, i32 num) {
  new_kobj(k);
  k->type = tuple;
  k->data.tuple.tups = tups;
  k->data.tuple.num = num;
  return k;
}

khagm_obj *create_ITE(khagm_obj ** ite) {
  new_kobj(k);
  k->type = ITE;
  k->data.ITE.ite = ite;
  return k;
}

khagm_obj *create_int(i64 i) {
  new_kobj(k);
  k->type = ub_int;
  k->data.unboxed_int = i;
  return k;
}

khagm_obj *create_float(f64 f) {
  new_kobj(k);
  k->type = ub_float;
  k->data.unboxed_float = f;
  return k;
}

khagm_obj *create_string(char * c) {
  i64 l = strlen(c);
  kstring ks;
  ks.data = c;
  ks.len = l;
  new_kobj(k);
  k->type = str;
  k->data.string = ks;
  return k;
}

khagm_obj *create_seq(khagm_obj *a, khagm_obj*b) {
  new_kobj(k);
  k->type = seq;
  k->data.seq.a = a;
  k->data.seq.b = b;
  return k;
}


khagm_obj ** create_list(i32 num, ...) {
  va_list ap;
  va_start(ap, num);
  khagm_obj ** arr = k_alloc(sizeof(khagm_obj*) * num);
  for (int i = 0; i < num; i++) {
    arr[i] = va_arg(ap, khagm_obj*);
  }
  va_end(ap);
  return arr;
}
