#include "type.h"
#include "gc.h"
#include "obj.h"
#include "call.h"
#include <stdio.h>
#include <stdlib.h>

extern u64 used;



KHASM_ENTRY(khasm_Stdlib775896895_s1597980479_eq, 2, kha_obj *b, kha_obj *c) {
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
   unref(b);
  unref(c);
  return ret;
}
KHASM_ENTRY(khasm_Stdlib775896895_iadd, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID ADDITION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i + c->data.i);
   unref(b);
  unref(c);
  return ret;
}


KHASM_ENTRY(khasm_Stdlib775896895_isub, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID SUBTRACTION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i - c->data.i);
   unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_Stdlib775896895_imul, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID MULT\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i * c->data.i);
   unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_Stdlib775896895_idiv, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID DIV\n");
    exit(1);
  }
  kha_obj * ret = make_int((i64) b->data.i / c->data.i);
   unref(b);
  unref(c);
  return ret;
}

// FLOAT STUFF
KHASM_ENTRY(khasm_Stdlib775896895_fadd, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FADDITION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f + c->data.f);
   unref(b);
  unref(c);
  return ret;
}


KHASM_ENTRY(khasm_Stdlib775896895_fsub, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FSUBTRACTION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f - c->data.f);
   unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_Stdlib775896895_fmul, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FMULT\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f * c->data.f);
   unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_Stdlib775896895_fdiv, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FDIV\n");
    exit(1);
  }
  kha_obj * ret = make_int((f64) b->data.f / c->data.f);
  unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_Stdlib775896895_print1597980479_int, 1, kha_obj *b) {
  if (b->tag != INT) {
    fprintf(stderr, "INVALID PRINT INT\n");
  }
  printf("%ld\n", b->data.i);
  unref(b);
  return make_tuple(0, NULL);
}
