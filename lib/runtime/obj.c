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

kha_obj * make_string(char * f) {
  kha_obj * k = new_kha_obj(STR);
  k->data.str = malloc(sizeof(struct kha_obj_str));
  k->data.str->data = strdup(f);
  k->data.str->len = strlen(f);
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
  kha_obj ** arr;
  if (num == 0) {
    arr = NULL;
  }
  else {
    va_list ap;
    va_start(ap, num);
    arr = malloc(sizeof(kha_obj*) * num);
    for (int i = 0; i < num; i++) {
      arr[i] = ref(va_arg(ap, kha_obj*));
    }
    va_end(ap);
  }
  kha_obj *k = new_kha_obj(TUPLE);
  k->data.tuple = malloc(sizeof(struct kha_obj_tuple));
  k->data.tuple->len = num;
  k->data.tuple->tups = arr;
  k->gc = 1;
  return k;
}

kha_obj *copy(kha_obj * a) {
  if (a->tag == PAP) {
    kha_obj * new = new_kha_obj(PAP);
    new->data.pap = malloc(sizeof(struct kha_obj_pap));
    new->data.pap->argnum
      = a->data.pap->argnum;
    new->data.pap->func
      = a->data.pap->func;
    new->data.pap->args
      = malloc(sizeof(kha_obj *)
	       * a->data.pap->argnum);
    memcpy(new->data.pap->args,
	   a->data.pap->args,
	   sizeof(kha_obj*)
	   * a->data.pap->argnum);
    for (int i = 0; i < a->data.pap->argnum; i++) {
      ref(new->data.pap->args[i]);
    }
    new->gc = 1;
    return new;
  }
  else {
    fprintf(stderr, "TODO: copy other stuff");
    exit(1);
  }
}
