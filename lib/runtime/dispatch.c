#include "dispatch.h"
khagm_obj *dispatch(int a, fptr p, khagm_obj **d) {
  switch (a) {
  case 0: {
    khagm_obj *(*f)() = p;
    return f();
  }

  case 1: {
    khagm_obj *(*f)(khagm_obj *) = p;
    return f(d[0]);
  }

  case 2: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1]);
  }

  case 3: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2]);
  }

  case 4: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3]);
  }

  case 5: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4]);
  }

  case 6: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5]);
  }

  case 7: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6]);
  }

  case 8: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7]);
  }

  case 9: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8]);
  }

  case 10: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9]);
  }

  case 11: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10]);
  }

  case 12: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11]);
  }

  case 13: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12]);
  }

  case 14: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13]);
  }

  case 15: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14]);
  }

  case 16: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15]);
  }

  case 17: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16]);
  }

  case 18: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17]);
  }

  case 19: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18]);
  }

  case 20: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19]);
  }

  case 21: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20]);
  }

  case 22: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21]);
  }

  case 23: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22]);
  }

  case 24: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23]);
  }

  case 25: {
    khagm_obj *(*f)(
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24]);
  }

  case 26: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25]);
  }

  case 27: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26]);
  }

  case 28: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26], d[27]);
  }

  case 29: {
    khagm_obj *(*f)(
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26], d[27], d[28]);
  }

  case 30: {
    khagm_obj *(*f)(
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
        khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26], d[27], d[28],
             d[29]);
  }

  case 31: {
    khagm_obj *(*f)(khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *, khagm_obj *,
                    khagm_obj *, khagm_obj *, khagm_obj *) = p;
    return f(d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9], d[10],
             d[11], d[12], d[13], d[14], d[15], d[16], d[17], d[18], d[19],
             d[20], d[21], d[22], d[23], d[24], d[25], d[26], d[27], d[28],
             d[29], d[30]);
  }
  default:
    throw_err("Too big arity", FATAL);
    return NULL;
  }
}
