#ifndef KHASM_GC
#define KHASM_GC
#include "type.h"
#include "obj.h"

kha_obj *new_kha_obj(kha_obj_typ t);

kha_obj *ref(kha_obj *);
void unref(kha_obj *);
void k_free(kha_obj *);
void k_thread_free(kha_obj *);
void free_worker(void);
void true_free(kha_obj *);
#endif
