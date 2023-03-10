#include "err.h"

void throw_err(const char * msg, enum err_status stat) {
  switch (stat) {
  case MINOR:
    printf("\nMINOR ERR: %s\n", msg);
    break;
  case MAJOR:
    printf("\nMAJOR ERR: %s\n", msg);
    break;
  case FATAL:
    printf("\nFATAL ERR: %s\n", msg);
    exit(1);
    break;
  }
}
