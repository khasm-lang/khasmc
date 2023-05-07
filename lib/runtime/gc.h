#ifndef KHASM_GC
#define KHASM_GC
#include "type.h"

typedef struct kha_heap {
  i64 left;
  void *data;
  struct heap *next;
} kha_heap;

#endif
