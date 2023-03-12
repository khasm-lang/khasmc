#include "err.h"

//Regular text
#define BLK "\e[0;30m"
#define RED "\e[0;31m"
#define GRN "\e[0;32m"
#define YEL "\e[0;33m"
#define BLU "\e[0;34m"
#define MAG "\e[0;35m"
#define CYN "\e[0;36m"
#define WHT "\e[0;37m"
//Reset
#define reset "\e[0m"
  
void throw_err(const char * msg, enum err_status stat) {
  switch (stat) {
  case MINOR:
    printf(YEL "\nMINOR ERR: %s\n" reset, msg);
    break;
  case MAJOR:
    printf(MAG "\nMAJOR ERR: %s\n" reset, msg);
    break;
  case FATAL:
    printf(RED "\nFATAL ERR: %s\n" reset, msg);
    exit(1);
    break;
  }
}
