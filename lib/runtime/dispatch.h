#pragma once
#include <stdlib.h>
#include <string.h>
#include "err.h"
#include "khagm_obj.h"
#include "khagm_alloc.h"

khagm_obj * dispatch(int, fptr, khagm_obj **);
