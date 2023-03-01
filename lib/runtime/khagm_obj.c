#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "types.h"

#define packed __attribute__ ((packed))

typedef struct kstring {
  char * data;
  i64 len;
} packed kstring;

typedef struct khagm_obj {
  enum {
    val,
    thunk,
    tuple,
    ub_int,
    ub_float,
    str,
  } type;
  union {
    fptr * val;
    
    struct {
      fptr * function;
      struct khagm_obj * args;
      i32 argnum;
    } packed thunk;
    
    struct khagm_obj * tuple;
    
    i64 unboxed_int;
    f64 unboxed_float;
    kstring * string;
  } packed data;
} khagm_obj;
