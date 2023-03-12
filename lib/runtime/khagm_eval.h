#pragma once
#include <stdio.h>
#include <stdlib.h>
#include "err.h"
#include "khagm_obj.h"
#include "khagm_alloc.h"
#include "dispatch.h"
khagm_obj *reconcile
(khagm_obj *ret, khagm_obj **args, int arity, int argnum);
khagm_obj * khagm_eval(khagm_obj * root);
