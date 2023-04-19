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
  i64 x1 = (i64)(*a->data.FULL);
  i64 y1 = (i64)(*b->data.FULL);
  i32 x2 = (i32)(*(a->data.FULL + 8));
  i32 y2 = (i32)(*(b->data.FULL + 8));
  return (x1 == y1) && (x2 == y2);

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

