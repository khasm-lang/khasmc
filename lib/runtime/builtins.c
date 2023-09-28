#include "type.h"
#include "gc.h"
#include "obj.h"
#include "call.h"
#include <stdio.h>
#include <stdlib.h>

KHASM_ENTRY(khasm_s_eq, 2, kha_obj *b, kha_obj *c) {
  kha_obj * ret;
  if (((u64)b & 1) == 1) {
    if (((u64)c & 1) == 1) {
      ret = make_int(b == c);
    }
    else {
      ret = make_int(0);
    }
  }
  else {
    if (b->tag != c->tag) {
      ret =  make_int(0);
    }
    else if (b->data.i != c->data.i) {
      ret =  make_int(0);
    }
    else {
      ret = make_int(1);
    }
  }
  unref(b);
  unref(c);
  return ret;
}
KHASM_ENTRY(khasm_int_add, 2, kha_obj *b, kha_obj *c) {
  kha_obj * ret = make_int(((u64)b >> 1) + ((u64)c >> 1));
  return ret;
}


KHASM_ENTRY(khasm_int_sub, 2, kha_obj *b, kha_obj *c) {
  kha_obj * ret = make_int(((u64)b >> 1) - ((u64)c >> 1));
  return ret;
}

KHASM_ENTRY(khasm_int_mul, 2, kha_obj *b, kha_obj *c) {
  kha_obj * ret = make_int(((u64)b >> 1) * ((u64)c >> 1));
  return ret;
}

KHASM_ENTRY(khasm_int_div, 2, kha_obj *b, kha_obj *c) {
  kha_obj * ret = make_int(((u64)b >> 1) / ((u64)c >> 1));
  return ret;
}

// FLOAT STUFF
KHASM_ENTRY(khasm_float_add, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FADDITION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f + c->data.f);
  unref(b);unref(c);

  return ret;
}


KHASM_ENTRY(khasm_float_sub, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FSUBTRACTION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f - c->data.f);
  unref(b);unref(c);
  return ret;
}

KHASM_ENTRY(khasm_float_mul, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FMULT\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f * c->data.f);
  unref(b);unref(c);

  return ret;
}

KHASM_ENTRY(khasm_float_div, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FDIV\n");
    exit(1);
  }
  kha_obj * ret = make_int((f64) b->data.f / c->data.f);
  unref(b);unref(c);

  return ret;
}

KHASM_ENTRY(khasm_print_int, 1, kha_obj *b) {
  printf("%ld\n", (u64)b >> 1);
  unref(b);
  return make_tuple(0, NULL);
}

KHASM_ENTRY(khasm_print_str, 1, kha_obj *b) {
  if (b->tag != STR) {
    fprintf(stderr, "INVALID PRINT STR\n");
  }
  printf("%s\n", b->data.str.data);
  unref(b);
  return make_tuple(0, NULL);
}

KHASM_ENTRY(khasm_tuple_acc, 2, kha_obj*b, kha_obj*t) {
  if (t->tag != TUPLE && t->tag != ADT) {
    fprintf(stderr, "CAN'T TUPACC NONTUP\n");
  }
  kha_obj *tmp = t->data.tuple.tups[(u64)b >>1];
  unref(b);
  return ref(tmp);
}

