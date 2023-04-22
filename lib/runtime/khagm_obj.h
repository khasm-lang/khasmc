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
  union khagm_obj_union {
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
      struct khagm_obj ** a;
    } packed seq;
    
    i64 unboxed_int;
    f64 unboxed_float;
    kstring * string;

    i8 FULL[12];
    
  } packed data;
  i32 used;
} khagm_obj;

#define khagm_eval(a) ((khagm_obj *(*)(khagm_obj*))(a)->jump_point)(a)

void pprint_khagm_obj(khagm_obj * a);
i32 khagm_obj_eq(khagm_obj *a, khagm_obj *b);
khagm_obj * set_used(khagm_obj *a, i32 b);
khagm_obj * set_gc(khagm_obj *a, i8 b);
i32 get_used(khagm_obj *a);
i8 get_gc(khagm_obj *a);
khagm_obj * khagm_obj_copy_thunk(khagm_obj *);
