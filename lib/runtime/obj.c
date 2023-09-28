#include <stdlib.h>
#include <stdarg.h>
#include "gc.h"

kha_obj * make_raw_ptr(void *p) {
  kha_obj * k = new_kha_obj(PTR);
  k->data.ptr = p;
  // init at zero refcount bc otherwise they don't get freed properly
  k->gc = 0 << 8 | k->tag;
  return k;
}

kha_obj * make_ptr(kha_obj * p) {
  kha_obj * k = new_kha_obj(PTR);
  k->data.ptr = ref(p);
  k->gc = 1 << 8 | k->tag;
  return k;
}

kha_obj * make_int(i64 i) {
  return (kha_obj*) ((i << 1) | 1);
}

kha_obj * make_float(f64 f) {
  kha_obj * k = new_kha_obj(FLOAT);
  k->data.f = f;
  k->gc = 1 << 8 | k->tag;
  return k;
}

kha_obj * make_string(char * f) {
  kha_obj * k = new_kha_obj(STR);
  k->data.str.data = strdup(f);
  k->data.str.len = strlen(f);
  k->gc = 1 << 8 | k->tag;
  return k;
}

kha_obj * make_pap(u64 argnum, void * p, kha_obj ** args) {
  kha_obj * k = new_kha_obj(PAP);
  k->data.pap = kha_alloc(sizeof(struct kha_obj_pap));
  k->data.pap->argnum = argnum;
  k->data.pap->args = args;
  k->data.pap->func = p;
  k->gc = 1 << 8 | k->tag;
  return k;
}

kha_obj * make_tuple(u64 num, ...) {
  kha_obj ** arr;
  if (num == 0) {
    arr = NULL;
  }
  else {
    va_list ap;
    va_start(ap, num);
    arr = kha_alloc(sizeof(kha_obj*) * (num + 1));
    arr[num] = 0;
    for (int i = 0; i < num; i++) {
      arr[i] = ref(va_arg(ap, kha_obj*));
    }
    va_end(ap);
  }
  kha_obj *k = new_kha_obj(TUPLE);
  k->data.tuple.len = num;
  k->data.tuple.tups = arr;
  k->gc = 1 << 8 | k->tag;
  return k;
}

kha_obj *copy(kha_obj * a) {
  if (a->tag == PAP) {
    kha_obj * new = new_kha_obj(PAP);
    new->data.pap = kha_alloc(sizeof(struct kha_obj_pap));
    new->data.pap->argnum
      = a->data.pap->argnum;
    new->data.pap->func
      = a->data.pap->func;
    new->data.pap->args
      = kha_alloc(sizeof(kha_obj *)
	       * a->data.pap->argnum);
    memcpy(new->data.pap->args,
	   a->data.pap->args,
	   sizeof(kha_obj*)
	   * a->data.pap->argnum);
    for (int i = 0; i < a->data.pap->argnum; i++) {
      ref(new->data.pap->args[i]);
    }
    new->gc = 1 << 8 | new->tag;
    return new;
  }
  else {
    fprintf(stderr, "TODO: copy other stuff");
    exit(1);
  }
}
