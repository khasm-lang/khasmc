#ifndef KHASM_GC
#define KHASM_GC
#include "type.h"
#include "obj.h"

kha_obj *new_kha_obj(kha_obj_typ t);

kha_obj *ref(kha_obj *);
void unref(kha_obj*);
#endif
