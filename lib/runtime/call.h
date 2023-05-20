#pragma once
#ifndef KHASM_CALL
#define KHASM_CALL
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "obj.h"
#include "gc.h"
#include "type.h"
kha_obj *call(kha_obj *f, kha_obj *x);


#endif
