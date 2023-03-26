#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"
#include "err.h"
#include "gc.h"

static long allocs = 0;
static long frees  = 0;


void * k_alloc(size_t n) {
  void * ret = GC_MALLOC(n);
  if (!ret) {
    throw_err("Alloc failed\n", FATAL);
  }
  allocs++;
  return ret;
}

void k_free(void * p) {
  frees++;
}

void * k_realloc(void * a, size_t n) {
  return GC_REALLOC(a, n);
}

long alloc_free_diff(void) {
  return allocs - frees;
}
