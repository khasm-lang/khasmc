#include "type.h"
#include "gc.h"
#include "obj.h"
#include <stdio.h>
#include <stdlib.h>

extern u64 used;

#define KHAFUNC(name) kha_obj *name(u64 i, kha_obj **a)


KHAFUNC(khasm_Stdlib775896895_s1597980479_eq) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
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
  return ret;
}

KHAFUNC(khasm_Stdlib775896895_iadd) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID ADDITION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i + c->data.i);
  return ret;
}


KHAFUNC(khasm_Stdlib775896895_isub) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID SUBTRACTION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i - c->data.i);
  return ret;
}

KHAFUNC(khasm_Stdlib775896895_imul) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID MULT\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.i * c->data.i);
  return ret;
}

KHAFUNC(khasm_Stdlib775896895_idiv) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID DIV\n");
    exit(1);
  }
  kha_obj * ret = make_int((i64) b->data.i / c->data.i);
  return ret;
}

// FLOAT STUFF
KHAFUNC(khasm_Stdlib775896895_fadd) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FADDITION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f + c->data.f);
  return ret;
}


KHAFUNC(khasm_Stdlib775896895_fsub) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FSUBTRACTION\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f - c->data.f);
  return ret;
}

KHAFUNC(khasm_Stdlib775896895_fmul) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FMULT\n");
    exit(1);
  }
  kha_obj * ret = make_int(b->data.f * c->data.f);
  return ret;
}

KHAFUNC(khasm_Stdlib775896895_fdiv) {
  if (i < 2) {
    return NULL;
  }
  used = 2;
  kha_obj * b = a[0];
  kha_obj * c = a[1];
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FDIV\n");
    exit(1);
  }
  kha_obj * ret = make_int((f64) b->data.f / c->data.f);
  return ret;
}

KHAFUNC(khasm_Stdlib775896895_print1597980479_int) {
  if (i < 1) {
    return NULL;
  }
  used = 1;
  
  kha_obj *b = a[0];
  if (b->tag != INT) {
    fprintf(stderr, "INVALID PRINT INT\n");
  }
  printf("%ld\n", b->data.i);
  return make_tuple(0, NULL);
}
