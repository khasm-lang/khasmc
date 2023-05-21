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
  if (!a) {
    // fprintf(stderr, "cannot ref nothing\n");
    exit(1);
  }
  a->gc += 1;
  //printf("ref  %d : %ld | %p\n",
  // 	 a->tag, a->gc, a);
  return a;
}

void unref(kha_obj * a) {
  if (!a) {
    return;
  }
  a->gc -= 1;
  //fprintf(stderr, "uref %d : %ld | %p\n",
  //  a->tag, a->gc, a);
  if (a->gc <= 0) {
    //fprintf(stderr, "free %d : %ld | %p\n",
    // 	    a->tag, a->gc, a);
    switch (a->tag) {
    case INT:
    case FLOAT:
    case ENUM:
    case PTR:
      free(a);
      break;
    case PAP: {
      for (int i = 0; i < a->data.pap->argnum; i++) {
unref(a->data.pap->args[i]);
      }
      free(a->data.pap->args);
      free(a->data.pap);
      free(a);
      break;
    }
    case ADT: {
      for (int i = 0; i < a->data.pap->argnum; i++) {
unref(a->data.adt->data[i]);
      }
      free(a->data.adt->data);
      free(a->data.adt);
      free(a);
      break;
    }
    case TUPLE: {
      for (int i = 0; i < a->data.tuple->len; i++) {
        unref(a->data.tuple->tups[i]);
      }
      free(a->data.tuple->tups);
      free(a->data.tuple);
      free(a);
      break;
    }
    case STR: {
      free(a->data.str->data);
      free(a->data.str);
      free(a);
      break;
    }
    case END: {
    fprintf(stderr, "UNREACHABLE\n");
    exit(1);
    }
    }

  }

}
