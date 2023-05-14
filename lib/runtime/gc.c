#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "gc.h"


kha_obj * new_kha_obj(kha_obj_typ t) {
  kha_obj * a = malloc(sizeof(kha_obj));
  a->tag = t;
  a->gc = 0;
  return a;
}


kha_obj * ref(kha_obj * a) {
  a->gc += 1;
  return a;
}

void unref(kha_obj * a) {
  a->gc -= 1;
  if (a->gc <= 0) {
    int i;
    switch (a->tag) {
    case INT:
    case FLOAT:
    case ENUM:
    case PTR:
      free(a);
    case PAP: {
      for (int i = 0; i < a->data.pap->argnum; i++) {
	unref(a->data.pap->args[i]);
      }
      free(a->data.pap);
      free(a);
    }
    case ADT: {
      for (int i = 0; i < a->data.pap->argnum; i++) {
	unref(a->data.adt->data[i]);
      }
      free(a->data.adt);
      free(a);
    }
    case STR:
      free(a->data.str->data);
      free(a->data.str);
      free(a);
    }
  }
}
