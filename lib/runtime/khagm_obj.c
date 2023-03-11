#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "types.h"
#include "khagm_obj.h"

#define packed __attribute__ ((packed))


extern long get_val_from_pointer(fptr p);

void pprint_khagm_obj(khagm_obj * p) {
  switch (p->type) {
  case val: {
    long a = get_val_from_pointer(p->data.val);
    printf("(Val %ld)\n", a);
    break;
  }
  case call: {
    long a = get_val_from_pointer(p->data.call.function);
    printf("(Call %ld\n", a);
    for (int i = 0; i < p->data.call.argnum; i++) {
      pprint_khagm_obj(p->data.call.args[i]);
    }
    printf(")\n");
    break;
  }
  case thunk: {
    printf("(Thunk \n");
    pprint_khagm_obj(p->data.thunk.function);
    for (int i = 0; i < p->data.thunk.argnum; i++) {
      pprint_khagm_obj(p->data.thunk.args[i]);
    }
    printf(")\n");
  }
  case tuple: {
    printf("(Tuple\n");
    for (int i = 0; i < p->data.tuple.num; i++) {
      pprint_khagm_obj(p->data.tuple.tups[i]);
    }
    printf(")");
    break;
  }
  case ub_int: {
    printf("(Int %ld)\n", p->data.unboxed_int);
    break;
  }
  case ub_float: {
    printf("(Float %.64f)\n", p->data.unboxed_float);
    break;
  }
  case str: {
    printf("(String ");
    for (int i = 0; i < p->data.string->len; i++) {
      printf("%d ", p->data.string->data[i]);
    }
    printf(")");
    break;
  }
  case ITE: {
    printf("(ITE\n");
    pprint_khagm_obj(p->data.ITE.ite[0]);
    pprint_khagm_obj(p->data.ITE.ite[1]);
    pprint_khagm_obj(p->data.ITE.ite[2]);
    printf(")\n");
  }
  }
}
