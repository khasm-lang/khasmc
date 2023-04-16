#pragma once
#include <stdio.h>
#include <stdlib.h>
#include "err.h"
#include "khagm_obj.h"
#include "khagm_alloc.h"
#include "dispatch.h"
khagm_obj *construct_thunk(khagm_obj *, khagm_obj **, i32);
khagm_obj *handle_thunk(khagm_obj *);
