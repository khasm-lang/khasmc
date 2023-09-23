#include "type.h"
#include "gc.h"
#include "obj.h"
#include "call.h"
#include <stdio.h>
#include <stdlib.h>

extern u64 used;



KHASM_ENTRY(kha_extern_96_s_95_eq, 2, kha_obj *b, kha_obj *c) {
  kha_obj * ret;
  if (b->tag != c->tag) {
    ret =  make_int(0);
  }
  else if (b->data.i != c->data.i) {
    ret =  make_int(0);
  }
  else {
    ret = make_int(1);
  }
    unref(b);unref(c);

  return ret;
}
KHASM_ENTRY(kha_extern_96_int_95_add, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID ADDITION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i + c->data.i);
  unref(b);unref(c);

  return ret;
}


KHASM_ENTRY(kha_extern_96_int_95_sub, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID SUBTRACTION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i - c->data.i);
  unref(b);unref(c);

  return ret;
}

KHASM_ENTRY(kha_extern_96_int_95_mul, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID MULT\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i * c->data.i);
  unref(b);unref(c);

  return ret;
}

KHASM_ENTRY(kha_extern_96_int_95_div, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID DIV\n");
    exit(1);
  }
  kha_obj * ret = make_int((i64) b->data.i / c->data.i);
  unref(b);unref(c);

  return ret;
}

// FLOAT STUFF
KHASM_ENTRY(kha_extern_96_float_95_add, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FADDITION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f + c->data.f);
  unref(b);unref(c);

  return ret;
}


KHASM_ENTRY(kha_extern_96_float_95_sub, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FSUBTRACTION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f - c->data.f);
  unref(b);unref(c);
  return ret;
}

KHASM_ENTRY(kha_extern_96_float_95_mul, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FMULT\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f * c->data.f);
  unref(b);unref(c);

  return ret;
}

KHASM_ENTRY(kha_extern_96_float_95_div, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FDIV\n");
    exit(1);
  }
  kha_obj * ret = make_int((f64) b->data.f / c->data.f);
  unref(b);unref(c);

  return ret;
}

KHASM_ENTRY(kha_extern_96_print_95_int, 1, kha_obj *b) {
  if (b->tag != INT) {
    fprintf(stderr, "INVALID PRINT INT\n");
  }
  printf("%ld\n", b->data.i);
  unref(b);
  return make_tuple(0, NULL);
}

KHASM_ENTRY(kha_extern_96_print_95_str, 1, kha_obj *b) {
  if (b->tag != STR) {
    fprintf(stderr, "INVALID PRINT STR\n");
  }
  printf("%s\n", b->data.str.data);
  unref(b);
  return make_tuple(0, NULL);
}

KHASM_ENTRY(khasm_tuple_acc, 2, kha_obj*b, kha_obj*t) {
  if (t->tag != TUPLE) {
    fprintf(stderr, "CAN'T TUPACC NONTUP\n");
  }
  if (b->tag != INT) {
    fprintf(stderr, "CAN'T TUPACC WITH NONINT\n");
  }
  kha_obj *tmp = t->data.tuple.tups[b->data.i];
  unref(b);
  return ref(tmp);
}

