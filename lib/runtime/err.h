#include <stdio.h>
#include <stdlib.h>

enum err_status {
  MINOR,
  MAJOR,
  FATAL,
};

void throw_err(const char * msg, enum err_status stat);
