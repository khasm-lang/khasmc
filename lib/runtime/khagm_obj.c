#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "types.h"
#include "khagm_obj.h"
#include "khagm_eval.h"

#define packed __attribute__ ((packed))


extern char * get_val_from_pointer(fptr p);

void pprint_khagm_obj(khagm_obj * p) {
  if (!p) {
    printf("(NULL)\n");
    return;
  }
  switch (p->type) {
  case val: {
    char * a = get_val_from_pointer(p->data.val);
    printf("(Val %s)\n", a);
    break;
  }
  case call: {
    char * a = get_val_from_pointer(p->data.call.function);
    printf("(Call %s\n", a);
    for (int i = 0; i < p->data.call.argnum; i++) {
      pprint_khagm_obj(p->data.call.args[i]);
    }
    printf(" %d)\n", p->data.call.argnum);
    break;
  }
  case thunk: {
    printf("(Thunk \n");
    pprint_khagm_obj(p->data.thunk.function);
    for (int i = 0; i < p->data.thunk.argnum; i++) {
      pprint_khagm_obj(p->data.thunk.args[i]);
    }
    printf(" %d)\n", p->data.thunk.argnum);
    break;
  }
  case seq: {
    printf("(Seq \n");
    pprint_khagm_obj(p->data.seq.a);
    pprint_khagm_obj(p->data.seq.b);
    printf(")\n");
    break;
  }
  case tuple: {
    printf("(Tuple\n");
    for (int i = 0; i < p->data.tuple.num; i++) {
      pprint_khagm_obj(p->data.tuple.tups[i]);
    }
    printf(")\n");
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
    for (int i = 0; i < p->data.string.len; i++) {
      printf("%c", p->data.string.data[i]);
    }
    printf(")\n");
    break;
  }
  case ITE: {
    printf("(ITE\n");
    pprint_khagm_obj(p->data.ITE.ite[0]);
    printf("\nTHEN\n");
    pprint_khagm_obj(p->data.ITE.ite[1]);
    printf("\nELSE\n");
    pprint_khagm_obj(p->data.ITE.ite[2]);
    printf(")\n");
    break;
  }
  default:
    printf("(UNKNOWN:\n");
    printf("\n)\n");
  }
}


int khagm_obj_eq(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type != b->type) {
    return 0;
  }
  if (a->type == ub_int) {
    return (a->data.unboxed_int
		      ==
		      b->data.unboxed_int);
  }
  else if (a->type == ub_float) {
    return (a->data.unboxed_float
		      ==
		      b->data.unboxed_float);
  }
  else if (a->type == tuple) {
    if (a->data.tuple.num != b->data.tuple.num) {
      return 0;
    }
    for (int i = 0; i < a->data.tuple.num; i++) {
      if (!khagm_obj_eq(a->data.tuple.tups[i],
			b->data.tuple.tups[i])) {
	return 0;
      }
    }
    return 1;
  }
  throw_err("Cannot compare types", FATAL);
  return -1;
}
