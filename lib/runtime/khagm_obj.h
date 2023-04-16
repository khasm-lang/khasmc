#pragma once
#include "types.h"
#include "khagm_alloc.h"
#define packed __attribute__ ((packed))

typedef struct kstring {
  char * data;
  i64 len;
} packed kstring;

/*
 */

typedef struct khagm_obj {
  fptr jump_point;
  union {
    fptr val;
    
    struct {
      struct khagm_obj ** args;
      i32 argnum;
    } packed callable;
    
    struct {
      struct khagm_obj ** tups;
      i32 num;
    } packed tuple;

    struct {
      struct khagm_obj * a;
      struct khagm_obj * b;
    } packed seq;
    
    i64 unboxed_int;
    f64 unboxed_float;
    kstring string;

    // i8 FULL[20];
    
  } packed data;
  gc_info GC_info;
  i32 used;
} khagm_obj;

void pprint_khagm_obj(khagm_obj * a);
int khagm_obj_eq(khagm_obj * a, khagm_obj * b);
