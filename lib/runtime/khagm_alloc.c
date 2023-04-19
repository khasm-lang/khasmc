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
  printf("alloc %4ld: %p\n", n, ret);
  return ret;
}

void k_free(void * p) {
  frees++;
}

void * k_realloc(void * a, size_t n) {
  void * p = GC_REALLOC(a, n);
  printf("realloc: %p -> %p\n", a, p);
  return p;
}

long alloc_free_diff(void) {
  return allocs - frees;
}
