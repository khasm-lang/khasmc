#pragma once
#include <stdio.h>
#include "types.h"
#include "khagm_alloc.h"
#include "khagm_obj.h"

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
