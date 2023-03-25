#pragma once
#include "types.h"
#include "khagm_alloc.h"
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
