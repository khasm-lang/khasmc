#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include "khagm_alloc.h"
#include "khagm_obj.h"
#include "khagm_eval.h"

#define new_kobj(nm) khagm_obj * nm = k_alloc(sizeof(khagm_obj)) 

khagm_obj * create_val(fptr f) {
  printf("making val %p\n", f);
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.val = f;
  return k;
}

khagm_obj *create_thunk
    (khagm_obj * function, khagm_obj ** args, i32 argnum) {
  new_kobj(k);
  k->jump_point = &handle_thunk;
  k->data.callable.args = args;
  k->data.callable.args[0] = function;
  k->data.callable.argnum = argnum;
  return k;
}

khagm_obj *create_call(fptr f, khagm_obj ** args, i32 argnum) {
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.val = f;
  return create_thunk(k, args, argnum);
}

khagm_obj *create_tuple(khagm_obj ** tups, i32 num) {
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.tuple.tups = tups;
  k->data.tuple.num = num;
  return k;
}

khagm_obj *create_int(i64 i) {
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.unboxed_int = i;
  return k;
}

khagm_obj *create_float(f64 f) {
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.unboxed_float = f;
  return k;
}

khagm_obj *create_string(char * c) {
  i64 l = strlen(c);
  kstring * ks = k_alloc(sizeof(kstring));
  ks->data = c;
  ks->len = l;
  new_kobj(k);
  k->jump_point = &handle_simpl;
  k->data.string = ks;
  return k;
}

khagm_obj *create_seq(khagm_obj *a, khagm_obj*b) {
  new_kobj(k);
  k->jump_point = &handle_seq;
  k->data.seq.a = k_alloc(sizeof(khagm_obj *) * 2);
  k->data.seq.a[0] = a;
  k->data.seq.a[1] = b;
  return k;
}


khagm_obj ** create_list(i32 num, ...) {
  va_list ap;
  va_start(ap, num);
  khagm_obj ** arr = k_alloc(sizeof(khagm_obj*) * (num + 1));
  for (int i = 1; i < num+1; i++) {
    arr[i] = va_arg(ap, khagm_obj*);
  }
  va_end(ap);
  return arr;
}
