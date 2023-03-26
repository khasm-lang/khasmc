#pragma once
#include "types.h"
#include <stdlib.h>

typedef enum gc_info {
  Reachable,
  Unreachable,
  Unmarked,
  ListStartReachable,
  ListStartUnreachable,
  ListStartUnmarked,
  ListContinue,
  ListEnd,
} __attribute__ ((packed)) gc_info;

void * k_alloc(size_t n);

void k_free(void * p);

void * k_realloc(void *, size_t);

long alloc_free_diff(void);
