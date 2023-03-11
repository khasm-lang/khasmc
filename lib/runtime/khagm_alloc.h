#include "types.h"
#include <stdlib.h>
void * k_alloc(size_t n);

void k_free(void * p);

long alloc_free_diff(void);
