#include "types.h"
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
    
    i64 unboxed_int;
    f64 unboxed_float;
    kstring string;
  } packed data;
  char GC_info;
} khagm_obj;
