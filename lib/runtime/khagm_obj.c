#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "types.h"
#include "khagm_obj.h"
#include "khagm_eval.h"

#define packed __attribute__ ((packed))


extern char * get_val_from_pointer(fptr p);

int khagm_obj_eq(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  // TODO: make this less stupid and dumb lmao
  return a->data.FULL == b->data.FULL;
}
