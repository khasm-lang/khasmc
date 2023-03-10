#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"
#include "err.h"

static long allocs = 0;
static long frees  = 0;

void * k_alloc(size_t n) {
  void * ret = calloc(1, n);
  if (!ret) {
    throw_err("Alloc failed\n", FATAL);
  }
  allocs++;
  return ret;
}

void k_free(void * p) {
  free(p);
  frees++;
}

long alloc_free_diff(void) {
  return allocs - frees;
}
