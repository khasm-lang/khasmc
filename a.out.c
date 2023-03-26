
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include "gc.h"  

#pragma once
#ifndef KHAGM_TYPES_H
#define KHAGM_TYPES_H
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;
typedef i64 *fptr;
#endif
#pragma once

typedef enum gc_info {
  Reachable,
  Unreachable,
  Unmarked,
  ListStartReachable,
  ListStartUnreachable,
  ListStartUnmarked,
  ListContinue,
  ListEnd,
} __attribute__ ((packed)) gc_info;

void * k_alloc(size_t n);

void k_free(void * p);

void * k_realloc(void *, size_t);

long alloc_free_diff(void);
#pragma once
#define packed __attribute__ ((packed))

typedef struct kstring {
  char * data;
  i64 len;
} packed kstring;

typedef struct khagm_obj {
  enum {
    val,
    call,
    thunk,
    tuple,
    ub_int,
    ub_float,
    str,
    ITE,
    seq,
  } packed type;
  union {
    fptr val;
    
    struct {
      fptr function;
      struct khagm_obj ** args;
      i32 argnum;
    } packed call;

    struct {
      struct khagm_obj * function;
      struct khagm_obj ** args;
      i32 argnum;
    } packed thunk;
    
    struct {
      struct khagm_obj ** tups;
      i32 num;
    } packed tuple;

    struct {
      struct khagm_obj ** ite;
    } ITE;

    struct {
      struct khagm_obj * a;
      struct khagm_obj * b;
    } seq;
    
    i64 unboxed_int;
    f64 unboxed_float;
    kstring string;

    i8 FULL[20];
    
  } packed data;
  gc_info GC_info;
} khagm_obj;

void pprint_khagm_obj(khagm_obj * a);
int khagm_obj_eq(khagm_obj * a, khagm_obj * b);
#pragma once
khagm_obj *reconcile
(khagm_obj *ret, khagm_obj **args, int arity, int argnum);
khagm_obj * khagm_eval(khagm_obj * root);
#pragma once

khagm_obj * create_val(fptr f);

khagm_obj *create_call
(fptr f, khagm_obj ** args, i32 argnum);

khagm_obj *create_thunk
(khagm_obj * function, khagm_obj ** args, i32 argnum);

khagm_obj *create_tuple(khagm_obj ** tups, i32 num);

khagm_obj *create_ITE(khagm_obj ** ite);

khagm_obj *create_int(i64 i);

khagm_obj *create_float(f64 f);

khagm_obj *create_string(char *c);

khagm_obj *create_seq(khagm_obj *a, khagm_obj*b);

khagm_obj ** create_list(i32 num, ...);
#pragma once

khagm_obj * dispatch(int, fptr, khagm_obj **);
#pragma once

enum err_status {
  MINOR,
  MAJOR,
  FATAL,
};

void throw_err(const char * msg, enum err_status stat);

#define new_kobj(nm) khagm_obj * nm = k_alloc(sizeof(khagm_obj))
khagm_obj * extern_1614757695_int1597980479_add(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int + b->data.unboxed_int;
    return k;
  }
  throw_err("attempted to add non ints", FATAL);
  return NULL;
}

khagm_obj *extern_1614757695_int1597980479_sub
    (khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int - b->data.unboxed_int;
    return k;
  }
  throw_err("attempted to sub non ints", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_int1597980479_mul(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = a->data.unboxed_int * b->data.unboxed_int;
    return k;
  }
  throw_err("attempted to mul non ints", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_int1597980479_div(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_int && b->type == ub_int) {
    new_kobj(k);
    k->type = ub_int;
    /* TODO: bounds checking? */
    k->data.unboxed_int = (i64) a->data.unboxed_int / b->data.unboxed_int;
    return k;
  }
  throw_err("attempted to div non ints", FATAL);
  return NULL;
}


khagm_obj * extern_1614757695_float1597980479_add(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float + b->data.unboxed_float;
    return k;
  }
  throw_err("attempted to add non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_sub(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float - b->data.unboxed_float;
    return k;
  }
  throw_err("attempted to sub non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_mul(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = a->data.unboxed_float * b->data.unboxed_float;
    return k;
  }
  throw_err("attempted to mul non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_float1597980479_div(khagm_obj * a, khagm_obj * b) {
  a = khagm_eval(a);
  b = khagm_eval(b);
  if (a->type == ub_float && b->type == ub_float) {
    new_kobj(k);
    k->type = ub_float;
    /* TODO: bounds checking? */
    k->data.unboxed_float = (f64) a->data.unboxed_float / b->data.unboxed_float;
    return k;
  }
  throw_err("attempted to div non floats", FATAL);
  return NULL;
}

khagm_obj * extern_1614757695_debug(khagm_obj * a) {
  khagm_obj * b = khagm_eval(a);
  pprint_khagm_obj(b);
  return create_tuple(create_list(0, NULL),0);
}


khagm_obj *extern_1614757695_s1597980479_eq
    (khagm_obj * a, khagm_obj * b) {
  if (khagm_obj_eq(a, b)) {
    return create_int(1);
  }
  return create_int(0);
}

khagm_obj *extern_1614757695_force(khagm_obj * a) {
  return khagm_eval(a);
}

#define new_kobj(nm) khagm_obj * nm = k_alloc(sizeof(khagm_obj)) 

khagm_obj * create_val(fptr f) {
  new_kobj(k);
  k->type = val;
  k->data.val = f;
  return k;
}

khagm_obj *create_call
    (fptr f, khagm_obj ** args, i32 argnum) {
  new_kobj(k);
  k->type = call;
  k->data.call.function = f;
  k->data.call.args = args;
  k->data.call.argnum = argnum;
  return k;
}

khagm_obj *create_thunk
    (khagm_obj * function, khagm_obj ** args, i32 argnum) {
  new_kobj(k);
  k->type = thunk;
  k->data.thunk.function = function;
  k->data.thunk.args = args;
  k->data.thunk.argnum = argnum;
  return k;
}

khagm_obj *create_tuple(khagm_obj ** tups, i32 num) {
  new_kobj(k);
  k->type = tuple;
  k->data.tuple.tups = tups;
  k->data.tuple.num = num;
  return k;
}

khagm_obj *create_ITE(khagm_obj ** ite) {
  new_kobj(k);
  k->type = ITE;
  k->data.ITE.ite = ite;
  return k;
}

khagm_obj *create_int(i64 i) {
  new_kobj(k);
  k->type = ub_int;
  k->data.unboxed_int = i;
  return k;
}

khagm_obj *create_float(f64 f) {
  new_kobj(k);
  k->type = ub_float;
  k->data.unboxed_float = f;
  return k;
}

khagm_obj *create_string(char * c) {
  i64 l = strlen(c);
  kstring ks;
  ks.data = c;
  ks.len = l;
  new_kobj(k);
  k->type = str;
  k->data.string = ks;
  return k;
}

khagm_obj *create_seq(khagm_obj *a, khagm_obj*b) {
  new_kobj(k);
  k->type = seq;
  k->data.seq.a = a;
  k->data.seq.b = b;
  return k;
}


khagm_obj ** create_list(i32 num, ...) {
  va_list ap;
  va_start(ap, num);
  khagm_obj ** arr = k_alloc(sizeof(khagm_obj*) * num);
  for (int i = 0; i < num; i++) {
    arr[i] = va_arg(ap, khagm_obj*);
  }
  va_end(ap);
  return arr;
}
khagm_obj *dispatch(int a, fptr p, khagm_obj **d) {
  switch (a) {
  case 0: {
    khagm_obj *(*f)() = p;
    return f();
  }

  case 1: {
    khagm_obj *(*f)(khagm_obj *) = p;
    return f(d[0]);
  }

  case 2: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1]);
  }

  case 3: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2]);
  }

  case 4: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3]);
  }

  case 5: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4]);
  }

  case 6: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5]);
  }

  case 7: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6]);
  }

  case 8: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7]);
  }

  case 9: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8]);
  }

  case 10: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9]);
  }

  case 11: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10]);
  }

  case 12: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11]);
  }

  case 13: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12]);
  }

  case 14: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13]);
  }

  case 15: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14]);
  }

  case 16: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15]);
  }

  case 17: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16]);
  }

  case 18: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17]);
  }

  case 19: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18]);
  }

  case 20: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19]);
  }

  case 21: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20]);
  }

  case 22: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21]);
  }

  case 23: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22]);
  }

  case 24: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23]);
  }

  case 25: {
    khagm_obj *(*f)(
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24]);
  }

  case 26: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25]);
  }

  case 27: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26]);
  }

  case 28: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26], d[27]);
  }

  case 29: {
    khagm_obj *(*f)(
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26], d[27], d[28]);
  }

  case 30: {
    khagm_obj *(*f)(
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26], d[27], d[28],
             d[29]);
  }

  case 31: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26], d[27], d[28],
             d[29], d[30]);
  }
  default:
    throw_err("Too big arity", FATAL);
    return NULL;
  }
}

//Regular text
#define BLK "\e[0;30m"
#define RED "\e[0;31m"
#define GRN "\e[0;32m"
#define YEL "\e[0;33m"
#define BLU "\e[0;34m"
#define MAG "\e[0;35m"
#define CYN "\e[0;36m"
#define WHT "\e[0;37m"
//Reset
#define reset "\e[0m"
  
void throw_err(const char * msg, enum err_status stat) {
  switch (stat) {
  case MINOR:
    printf(YEL "\nMINOR ERR: %s\n" reset, msg);
    break;
  case MAJOR:
    printf(MAG "\nMAJOR ERR: %s\n" reset, msg);
    break;
  case FATAL:
    printf(RED "\nFATAL ERR: %s\n" reset, msg);
    exit(1);
    break;
  }
}

static long allocs = 0;
static long frees  = 0;


void * k_alloc(size_t n) {
  void * ret = GC_MALLOC(n);
  if (!ret) {
    throw_err("Alloc failed\n", FATAL);
  }
  allocs++;
  return ret;
}

void k_free(void * p) {
  frees++;
}

void * k_realloc(void * a, size_t n) {
  return GC_REALLOC(a, n);
}

long alloc_free_diff(void) {
  return allocs - frees;
}
extern int khasm_get_argnum;
extern int arity_table(fptr);

khagm_obj * reconcile(khagm_obj * ret, khagm_obj ** args,
		      int arity, int argnum) {
  khagm_obj ** offset_args = args + arity;
  i32 offset = argnum - arity;
  switch (ret->type) {
  case call: {
    i32 ret_argnum = ret->data.thunk.argnum;
    ret->data.call.argnum += offset;
    ret->data.call.args = k_realloc(ret->data.call.args,
				  sizeof(khagm_obj*)
				  * ret->data.call.argnum);
    memcpy(ret->data.call.args + ret_argnum,
	   args + arity, offset * sizeof(khagm_obj*));
    return khagm_eval(ret);
  }
  case thunk: {
    i32 ret_argnum = ret->data.thunk.argnum;
    ret->data.thunk.argnum += offset;
    ret->data.thunk.args = k_realloc(ret->data.thunk.args,
				  sizeof(khagm_obj*)
				  * ret->data.thunk.argnum);
    memcpy(ret->data.thunk.args + ret_argnum,
	   args + arity, offset * sizeof(khagm_obj*));
    return khagm_eval(ret);
  }
  case val: {
    khagm_obj * new = k_alloc(sizeof(khagm_obj));
    new->type = call;
    new->data.call.args = k_alloc(sizeof(khagm_obj*) * offset);
    memcpy(new->data.call.args, args + arity, offset * sizeof(khagm_obj*));
    new->data.call.argnum = offset;
    return khagm_eval(new);
  }
  default: {
    throw_err("Cannot eval non-function type\n", FATAL);
    return NULL;
  }
  }
}


khagm_obj * khagm_eval(khagm_obj * root) {
  if (!root) {
    return NULL;
  }
  switch (root->type) {
  case val:
  case str:
  case ub_float:
  case ub_int:
    return root;
    break;
  case tuple: {
    for (int i = 0; i < root->data.tuple.num; i++) {
      root->data.tuple.tups[i] =
	khagm_eval(root->data.tuple.tups[i]);
    }
    return root;
    break;
  }
  case call: {
    /*
    for (int i = 0; i < root->data.call.argnum; i++) {
      root->data.call.args[i] =
	khagm_eval(root->data.call.args[i]);
	} */
    int arity = arity_table(root->data.call.function);
    if (arity == -1) {
      throw_err("Invalid function pointer\n", FATAL);
    }
    if (root->data.call.argnum < arity) {
      // not saturated yet
      return root;
    }
    khagm_obj * ret = dispatch(arity,
			       root->data.call.function,
			       root->data.call.args);
    if (root->data.call.argnum > arity) {
      khagm_obj * tmp = reconcile(ret, root->data.call.args,
		       arity, root->data.call.argnum);
      return tmp;
    }
    else {
      k_free(root->data.call.args);
      k_free(root);
      return khagm_eval(ret);
    }
  }
  case thunk: {
    /*
    for (int i = 0; i < root->data.call.argnum; i++) {
      root->data.call.args[i] =
	khagm_eval(root->data.call.args[i]);
	} */
    khagm_obj * ret = khagm_eval(root->data.thunk.function);
    return reconcile(ret,
		     root->data.thunk.args,
		     0,
		     root->data.thunk.argnum);
  }
  case ITE: {
    khagm_obj * ret = khagm_eval(root->data.ITE.ite[0]);
    if (ret->type != ub_int) {
      throw_err("ITE not boolean\n", FATAL);
    }
    if (ret->data.unboxed_int == 1) {
      return khagm_eval(root->data.ITE.ite[1]);
    }
    if (ret->data.unboxed_int == 0) {
      return khagm_eval(root->data.ITE.ite[2]);
    }
    throw_err("ITE not 0/1\n", FATAL);
  }
  case seq: {
    khagm_eval(root->data.seq.a);
    return khagm_eval(root->data.seq.b);
  }
  default:
    printf("ERROR TYPE: %d\n", root->type);
    throw_err("UNREACHABLE: khagm_eval", MAJOR);
    return NULL;
  }
}

void pprint_khagm_obj(khagm_obj * p);

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

  /* Compiler generated khasm code,
    running on the Khagm graph backend. */
#define khasm_Stdlib775896895_iadd extern_1614757695_int1597980479_add
/* -------- */
#define khasm_Stdlib775896895_isub extern_1614757695_int1597980479_sub
/* -------- */
#define khasm_Stdlib775896895_imul extern_1614757695_int1597980479_mul
/* -------- */
#define khasm_Stdlib775896895_idiv extern_1614757695_int1597980479_div
/* -------- */
#define khasm_Stdlib775896895_fadd extern_1614757695_float1597980479_add
/* -------- */
#define khasm_Stdlib775896895_fsub extern_1614757695_float1597980479_sub
/* -------- */
#define khasm_Stdlib775896895_fdiv extern_1614757695_float1597980479_div
/* -------- */
#define khasm_Stdlib775896895_fmul extern_1614757695_float1597980479_mul
/* -------- */
#define khasm_Stdlib775896895_debug extern_1614757695_debug
/* -------- */
#define khasm_Stdlib775896895_force extern_1614757695_force
/* -------- */
#define khasm_Stdlib775896895_s1597980479_eq extern_1614757695_s1597980479_eq
/* -------- */

/* Stdlib.= */
khagm_obj * khasm_Stdlib775896895_1027555135_() {

return create_call(&khasm_Stdlib775896895_s1597980479_eq, NULL, 0)
;
}
/* -------- */

/* Stdlib.+ */
khagm_obj * khasm_Stdlib775896895_725565247_() {

return create_call(&khasm_Stdlib775896895_iadd, NULL, 0)
;
}
/* -------- */

/* Stdlib.- */
khagm_obj * khasm_Stdlib775896895_759119679_() {

return create_call(&khasm_Stdlib775896895_isub, NULL, 0)
;
}
/* -------- */

/* Stdlib./ */
khagm_obj * khasm_Stdlib775896895_792674111_() {

return create_call(&khasm_Stdlib775896895_idiv, NULL, 0)
;
}
/* -------- */

/* Stdlib.* */
khagm_obj * khasm_Stdlib775896895_708788031_() {

return create_call(&khasm_Stdlib775896895_imul, NULL, 0)
;
}
/* -------- */

/* Stdlib.pipe */
khagm_obj * khasm_Stdlib775896895_pipe(khagm_obj * khasm_29, khagm_obj * khasm_30) {

return create_thunk(khasm_30,
 create_list(1,
 (khasm_29
)), 1)
;
}
/* -------- */

/* Stdlib.|> */
khagm_obj * khasm_Stdlib775896895_2084519743_1044332351_() {

return create_call(&khasm_Stdlib775896895_pipe, NULL, 0)
;
}
/* -------- */

/* Stdlib.apply */
khagm_obj * khasm_Stdlib775896895_apply(khagm_obj * khasm_33, khagm_obj * khasm_34) {

return create_thunk(khasm_33,
 create_list(1,
 (khasm_34
)), 1)
;
}
/* -------- */

/* Stdlib.$ */
khagm_obj * khasm_Stdlib775896895_608124735_() {

return create_call(&khasm_Stdlib775896895_apply, NULL, 0)
;
}
/* -------- */

/* Stdlib.compose */
khagm_obj * khasm_Stdlib775896895_compose(khagm_obj * khasm_37, khagm_obj * khasm_38, khagm_obj * khasm_39) {

return create_thunk(khasm_37,
 create_list(1,
 (create_thunk(khasm_38,
 create_list(1,
 (khasm_39
)), 1)
)), 1)
;
}
/* -------- */

/* Stdlib.% */
khagm_obj * khasm_Stdlib775896895_624901951_() {

return create_call(&khasm_Stdlib775896895_compose, NULL, 0)
;
}
/* -------- */

/* Stdlib.rcompose */
khagm_obj * khasm_Stdlib775896895_rcompose(khagm_obj * khasm_42, khagm_obj * khasm_43, khagm_obj * khasm_44) {

return create_thunk(khasm_43,
 create_list(1,
 (create_thunk(khasm_42,
 create_list(1,
 (khasm_44
)), 1)
)), 1)
;
}
/* -------- */

/* Stdlib.%> */
khagm_obj * khasm_Stdlib775896895_624901951_1044332351_() {

return create_call(&khasm_Stdlib775896895_rcompose, NULL, 0)
;
}
/* -------- */

/* Fib.fib */
khagm_obj * khasm_Fib775896895_fib(khagm_obj * khasm_47) {

return create_ITE(create_list(3,
 create_thunk(create_call(&khasm_Stdlib775896895_1027555135_,create_list(1,
 (khasm_47
)), 1)
, create_list(1,
 (create_int(0)

)), 1)
,
 khasm_47
,
 create_ITE(create_list(3,
 create_thunk(create_call(&khasm_Stdlib775896895_1027555135_,create_list(1,
 (khasm_47
)), 1)
, create_list(1,
 (create_int(1)

)), 1)
,
 khasm_47
,
 create_thunk(create_call(&khasm_Stdlib775896895_725565247_,create_list(1,
 (create_call(&khasm_Fib775896895_fib,create_list(1,
 (create_thunk(create_call(&khasm_Stdlib775896895_759119679_,create_list(1,
 (khasm_47
)), 1)
, create_list(1,
 (create_int(1)

)), 1)
)), 1)
)), 1)
, create_list(1,
 (create_call(&khasm_Fib775896895_fib,create_list(1,
 (create_thunk(create_call(&khasm_Stdlib775896895_759119679_,create_list(1,
 (khasm_47
)), 1)
, create_list(1,
 (create_int(2)

)), 1)
)), 1)
)), 1)
))
))
;
}
/* -------- */

/* main */
khagm_obj * main_____Khasm(khagm_obj * khasm_48) {

return create_call(&khasm_Stdlib775896895_debug,create_list(1,
 (create_call(&khasm_Fib775896895_fib,create_list(1,
 (create_int(15)

)), 1)
)), 1)
;
}int arity_table(fptr f) {
if (f == &extern_1614757695_int1597980479_add) return 2;
if (f == &extern_1614757695_int1597980479_sub) return 2;
if (f == &extern_1614757695_int1597980479_mul) return 2;
if (f == &extern_1614757695_int1597980479_div) return 2;
if (f == &extern_1614757695_float1597980479_add) return 2;
if (f == &extern_1614757695_float1597980479_sub) return 2;
if (f == &extern_1614757695_float1597980479_div) return 2;
if (f == &extern_1614757695_float1597980479_mul) return 2;
if (f == &extern_1614757695_debug) return 1;
if (f == &extern_1614757695_force) return 1;
if (f == &extern_1614757695_s1597980479_eq) return 2;
if (f == &khasm_Stdlib775896895_1027555135_) return 0;
if (f == &khasm_Stdlib775896895_725565247_) return 0;
if (f == &khasm_Stdlib775896895_759119679_) return 0;
if (f == &khasm_Stdlib775896895_792674111_) return 0;
if (f == &khasm_Stdlib775896895_708788031_) return 0;
if (f == &khasm_Stdlib775896895_pipe) return 2;
if (f == &khasm_Stdlib775896895_2084519743_1044332351_) return 0;
if (f == &khasm_Stdlib775896895_apply) return 2;
if (f == &khasm_Stdlib775896895_608124735_) return 0;
if (f == &khasm_Stdlib775896895_compose) return 3;
if (f == &khasm_Stdlib775896895_624901951_) return 0;
if (f == &khasm_Stdlib775896895_rcompose) return 3;
if (f == &khasm_Stdlib775896895_624901951_1044332351_) return 0;
if (f == &khasm_Fib775896895_fib) return 1;
if (f == &main_____Khasm) return 1;
return -1;
}char * get_val_from_pointer(fptr f){
if (f == &extern_1614757695_int1597980479_add) return "`int_add";
if (f == &extern_1614757695_int1597980479_sub) return "`int_sub";
if (f == &extern_1614757695_int1597980479_mul) return "`int_mul";
if (f == &extern_1614757695_int1597980479_div) return "`int_div";
if (f == &extern_1614757695_float1597980479_add) return "`float_add";
if (f == &extern_1614757695_float1597980479_sub) return "`float_sub";
if (f == &extern_1614757695_float1597980479_div) return "`float_div";
if (f == &extern_1614757695_float1597980479_mul) return "`float_mul";
if (f == &extern_1614757695_debug) return "`debug";
if (f == &extern_1614757695_force) return "`force";
if (f == &extern_1614757695_s1597980479_eq) return "`s_eq";
if (f == &khasm_Stdlib775896895_1027555135_) return "khasm_Stdlib775896895_1027555135_";
if (f == &khasm_Stdlib775896895_725565247_) return "khasm_Stdlib775896895_725565247_";
if (f == &khasm_Stdlib775896895_759119679_) return "khasm_Stdlib775896895_759119679_";
if (f == &khasm_Stdlib775896895_792674111_) return "khasm_Stdlib775896895_792674111_";
if (f == &khasm_Stdlib775896895_708788031_) return "khasm_Stdlib775896895_708788031_";
if (f == &khasm_Stdlib775896895_pipe) return "khasm_Stdlib775896895_pipe";
if (f == &khasm_Stdlib775896895_2084519743_1044332351_) return "khasm_Stdlib775896895_2084519743_1044332351_";
if (f == &khasm_Stdlib775896895_apply) return "khasm_Stdlib775896895_apply";
if (f == &khasm_Stdlib775896895_608124735_) return "khasm_Stdlib775896895_608124735_";
if (f == &khasm_Stdlib775896895_compose) return "khasm_Stdlib775896895_compose";
if (f == &khasm_Stdlib775896895_624901951_) return "khasm_Stdlib775896895_624901951_";
if (f == &khasm_Stdlib775896895_rcompose) return "khasm_Stdlib775896895_rcompose";
if (f == &khasm_Stdlib775896895_624901951_1044332351_) return "khasm_Stdlib775896895_624901951_1044332351_";
if (f == &khasm_Fib775896895_fib) return "khasm_Fib775896895_fib";
if (f == &main_____Khasm) return "main_____Khasm";
; return "NO NAME";
}
int main(void) {
  GC_INIT();  
  khagm_obj * m = main_____Khasm(NULL);
  khagm_eval(m);
  printf("DIFF: %d\n", alloc_free_diff());
}

