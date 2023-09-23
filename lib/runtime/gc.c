#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "gc.h"

void * kha_alloc(size_t sz) {
  return malloc(sz);
}

kha_obj * new_kha_obj(kha_obj_typ t) {
  kha_obj * a = kha_alloc(sizeof(kha_obj));
  a->tag = t;
  // a->gc = 0;
  return a;
}


inline kha_obj * ref(kha_obj * a) {
  if (!a) {
    fprintf(stderr, "can't ref null\n");
    exit(1);
  }
  a->gc += 1 << 8;
  return a;
}

inline void unref(kha_obj * a) {
  if (!a) {
    return;
  }
  a->gc -= 1 << 8;
  if (a->gc >> 8 <= 0) {
    k_free(a);
  }
}

void k_free(kha_obj * a) {
  if (!a) return;
  switch (a->tag & 0xF) {
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
	unref(a->data.adt.data[i]);
      }
      free(a->data.adt.data);
      free(a);
      break;
    }
    case TUPLE: {
      for (int i = 0; i < a->data.tuple.len; i++) {
        unref(a->data.tuple.tups[i]);
      }
      if (a->data.tuple.tups)
	free(a->data.tuple.tups);
      free(a);
      break;
    }
    case STR: {
      free(a->data.str.data);
      free(a);
      break;
    }
  default:
  case END: {
    fprintf(stderr, "UNREACHABLE???\n");
    exit(1);
  }
  }
}
