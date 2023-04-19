#include <gc/gc.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"
#include "err.h"
#include "gc.h"

static long allocs = 0;
static long frees  = 0;

#define KMALLOC GC_MALLOC
#define KREALLOC GC_REALLOC

void * k_alloc(size_t n) {
  void * ret = KMALLOC(n);
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
  void * p = KREALLOC(a, n);
  return p;
}

long alloc_free_diff(void) {
  return allocs - frees;
}
