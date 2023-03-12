#pragma once
#include <stdlib.h>
#include <string.h>
#include "err.h"
#include "khagm_obj.h"
#include "khagm_alloc.h"
khagm_obj * reconcile(khagm_obj * ret, khagm_obj ** args,
		      int arity, int argnum);
khagm_obj * dispatch(int, fptr, khagm_obj **);
