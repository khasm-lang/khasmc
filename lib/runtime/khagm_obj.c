#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "types.h"
#include "khagm_obj.h"
#include "khagm_eval.h"

#define packed __attribute__ ((packed))


extern char * get_val_from_pointer(fptr p);

i32 khagm_obj_eq(khagm_obj * a, khagm_obj * b) {
  // TODO: make this less stupid and dumb lmao
  return a->data.FULL == b->data.FULL;
}
khagm_obj * set_used(khagm_obj *a, i32 b) {
  a->used = b;
  return a;
}
khagm_obj * set_gc(khagm_obj *a, i8 b) {
  i32 mask = ((1 << 24) - 1) << 8;
  i32 first24 = a->used & mask;
  a->used = b & (first24 << 8);
  return a;
}

i32 get_used(khagm_obj *a) { return a->used; }

i8 get_gc(khagm_obj *a) { return a->used & (1 << 8) - 1; }

