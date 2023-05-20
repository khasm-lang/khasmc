#include <stdlib.h>
#include <stdarg.h>
#include "gc.h"

kha_obj * make_raw_ptr(void *p) {
  kha_obj * k = new_kha_obj(PTR);
  k->data.ptr = p;
  k->gc = 1;
  return k;
}

kha_obj * make_ptr(kha_obj * p) {
  kha_obj * k = new_kha_obj(PTR);
  k->data.ptr = ref(p);
  k->gc = 1;
  return k;
}

kha_obj * make_int(i64 i) {
  kha_obj * k = new_kha_obj(INT);
  k->data.i = i;
  k->gc = 1;
  return k;
}

kha_obj * make_float(f64 f) {
  kha_obj * k = new_kha_obj(FLOAT);
  k->data.f = f;
  k->gc = 1;
  return k;
}

kha_obj * make_pap(u64 argnum, void * p, kha_obj ** args) {
  kha_obj * k = new_kha_obj(PAP);
  k->data.pap = malloc(sizeof(struct kha_obj_pap));
  k->data.pap->argnum = argnum;
  k->data.pap->args = args;
  k->data.pap->func = p;
  k->gc = 1;
  return k;
}

kha_obj * make_tuple(u64 num, ...) {

  
  
  va_list ap;
  va_start(ap, num);
  kha_obj ** arr = malloc(sizeof(kha_obj*) * num);
  for (int i = 0; i < num; i++) {
    arr[i] = ref(va_arg(ap, kha_obj*));
  }
  va_end(ap);
  
  kha_obj *k = new_kha_obj(TUPLE);
  k->data.pap = malloc(sizeof(struct kha_obj_tuple));
  k->data.tuple->len = num;
  k->data.tuple->tups = arr;
  k->gc = 1;
  return k;
}
