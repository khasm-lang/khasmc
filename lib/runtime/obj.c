#include "gc.h"
kha_obj * make_ptr(kha_obj * p) {
  kha_obj * k = new_kha_obj(PTR);
  k->data.ptr = p;
  p->gc++;
  return k;
}

kha_obj * make_int(i64 i) {
  kha_obj * k = new_kha_obj(INT);
  k->data.i = i;
  return k;
}

kha_obj * make_float(f64 f) {
  kha_obj * k = new_kha_obj(FLOAT);
  k->data.f = f;
  return k;
}
