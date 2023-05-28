#include "call.h"

extern u64 used;

kha_obj * add_arg(kha_obj *a, kha_obj *b) {
  if (a->tag == PAP) {
    a->data.pap->argnum++;
    a->data.pap->args =
        realloc(a->data.pap->args, a->data.pap->argnum * sizeof(kha_obj *));
    a->data.pap->args[a->data.pap->argnum - 1] = ref(b);
    MUSTTAIL
    return call(a);
  } else if (a->tag == PTR) {
    kha_obj **args = malloc(sizeof(kha_obj *));
    args[0] = ref(b);
    kha_obj *k = make_pap(1, a->data.ptr, args);
    unref(a);
    MUSTTAIL
    return call(k);
  } else {
    fprintf(stderr, "ERROR: Can't call non-ptr %d", a->tag);
    exit(1);
  }
}

kha_obj *reconcile(u64 arg, kha_obj *pap, kha_obj *ret) {
  u64 extra = pap->data.pap->argnum - arg;
  if (ret->tag == PAP) {
    ret->data.pap->args = realloc(ret->data.pap->args,
				  sizeof(kha_obj *)
				  * (ret->data.pap->argnum + extra));
    for(int i = ret->data.pap->argnum; i < ret->data.pap->argnum + extra; i++) {
      ret->data.pap->args[i] = ref(pap->data.pap->args[i]);
    }
    unref(pap);
    MUSTTAIL
      return call(ret);
  }
  else if (ret->tag == PTR) {
    kha_obj **args = malloc(sizeof(kha_obj *) * (pap->data.pap->argnum - arg));
    for (int i = 0; i < (pap->data.pap->argnum - arg); i++) {
      args[i] = ref(pap->data.pap->args[i]);
    }
    kha_obj * new = make_pap(pap->data.pap->argnum - arg, ret->data.ptr, args);
    unref(ret);
    unref(pap);
    MUSTTAIL
      return call(new);
  }
  else {
    fprintf(stderr, "Cannot reconcile: %d\n", ret->tag);
    exit(1);
  }
}

kha_obj *call(kha_obj *a) {
  GET_ARGS(argnum, a->data.pap->func);
  void *(*func)(void) = a->data.pap->func;
  if (a->data.pap->argnum < argnum) {
    return a;
  } else if (a->data.pap->argnum == argnum) {
    switch (argnum) {
    case 0: {
      kha_obj *(*f)() = func;
      return MUSTTAIL f();
    }

    case 1: {
      kha_obj *(*f)(kha_obj *) = func;
      return MUSTTAIL f(a->data.pap->args[0]);
    }

    case 2: {
      kha_obj *(*f)(kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(a->data.pap->args[0], a->data.pap->args[1]);
    }

    case 3: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(a->data.pap->args[0], a->data.pap->args[1],
                        a->data.pap->args[2]);
    }

    case 4: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(a->data.pap->args[0], a->data.pap->args[1],
                        a->data.pap->args[2], a->data.pap->args[3]);
    }

    case 5: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) =
          func;
      return MUSTTAIL f(a->data.pap->args[0], a->data.pap->args[1],
                        a->data.pap->args[2], a->data.pap->args[3],
                        a->data.pap->args[4]);
    }

    case 6: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *) = func;
      return MUSTTAIL f(a->data.pap->args[0], a->data.pap->args[1],
                        a->data.pap->args[2], a->data.pap->args[3],
                        a->data.pap->args[4], a->data.pap->args[5]);
    }

    case 7: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(a->data.pap->args[0], a->data.pap->args[1],
                        a->data.pap->args[2], a->data.pap->args[3],
                        a->data.pap->args[4], a->data.pap->args[5],
                        a->data.pap->args[6]);
    }

    case 8: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(a->data.pap->args[0], a->data.pap->args[1],
                        a->data.pap->args[2], a->data.pap->args[3],
                        a->data.pap->args[4], a->data.pap->args[5],
                        a->data.pap->args[6], a->data.pap->args[7]);
    }

    case 9: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(
          a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8]);
    }

    case 10: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) =
          func;
      return MUSTTAIL f(a->data.pap->args[0], a->data.pap->args[1],
                        a->data.pap->args[2], a->data.pap->args[3],
                        a->data.pap->args[4], a->data.pap->args[5],
                        a->data.pap->args[6], a->data.pap->args[7],
                        a->data.pap->args[8], a->data.pap->args[9]);
    }

    case 11: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *) = func;
      return MUSTTAIL f(
          a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10]);
    }

    case 12: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(
          a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10], a->data.pap->args[11]);
    }

    case 13: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(
          a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10], a->data.pap->args[11],
          a->data.pap->args[12]);
    }

    case 14: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      return MUSTTAIL f(
          a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10], a->data.pap->args[11],
          a->data.pap->args[12], a->data.pap->args[13]);
    }

    case 15: {
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) =
          func;
      return MUSTTAIL f(
          a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10], a->data.pap->args[11],
          a->data.pap->args[12], a->data.pap->args[13], a->data.pap->args[14]);
    }
    }
  }
  switch (argnum) {
  case 0: {
    kha_obj *(*f)() = func;
    return MUSTTAIL reconcile(argnum, a, f());
  }

  case 1: {
    kha_obj *(*f)(kha_obj *) = func;
    return MUSTTAIL reconcile(argnum, a, f(a->data.pap->args[0]));
  }

  case 2: {
    kha_obj *(*f)(kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(argnum, a,
                              f(a->data.pap->args[0], a->data.pap->args[1]));
  }

  case 3: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(
        argnum, a,
        f(a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2]));
  }

  case 4: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(argnum, a,
                              f(a->data.pap->args[0], a->data.pap->args[1],
                                a->data.pap->args[2], a->data.pap->args[3]));
  }

  case 5: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(argnum, a,
                              f(a->data.pap->args[0], a->data.pap->args[1],
                                a->data.pap->args[2], a->data.pap->args[3],
                                a->data.pap->args[4]));
  }

  case 6: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *) = func;
    return MUSTTAIL reconcile(argnum, a,
                              f(a->data.pap->args[0], a->data.pap->args[1],
                                a->data.pap->args[2], a->data.pap->args[3],
                                a->data.pap->args[4], a->data.pap->args[5]));
  }

  case 7: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(argnum, a,
                              f(a->data.pap->args[0], a->data.pap->args[1],
                                a->data.pap->args[2], a->data.pap->args[3],
                                a->data.pap->args[4], a->data.pap->args[5],
                                a->data.pap->args[6]));
  }

  case 8: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(argnum, a,
                              f(a->data.pap->args[0], a->data.pap->args[1],
                                a->data.pap->args[2], a->data.pap->args[3],
                                a->data.pap->args[4], a->data.pap->args[5],
                                a->data.pap->args[6], a->data.pap->args[7]));
  }

  case 9: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(
        argnum, a,
        f(a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8]));
  }

  case 10: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(argnum, a,
                              f(a->data.pap->args[0], a->data.pap->args[1],
                                a->data.pap->args[2], a->data.pap->args[3],
                                a->data.pap->args[4], a->data.pap->args[5],
                                a->data.pap->args[6], a->data.pap->args[7],
                                a->data.pap->args[8], a->data.pap->args[9]));
  }

  case 11: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *) = func;
    return MUSTTAIL reconcile(
        argnum, a,
        f(a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10]));
  }

  case 12: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(
        argnum, a,
        f(a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10], a->data.pap->args[11]));
  }

  case 13: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(
        argnum, a,
        f(a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10], a->data.pap->args[11],
          a->data.pap->args[12]));
  }

  case 14: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(
        argnum, a,
        f(a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10], a->data.pap->args[11],
          a->data.pap->args[12], a->data.pap->args[13]));
  }

  case 15: {
    kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
                  kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
    return MUSTTAIL reconcile(
        argnum, a,
        f(a->data.pap->args[0], a->data.pap->args[1], a->data.pap->args[2],
          a->data.pap->args[3], a->data.pap->args[4], a->data.pap->args[5],
          a->data.pap->args[6], a->data.pap->args[7], a->data.pap->args[8],
          a->data.pap->args[9], a->data.pap->args[10], a->data.pap->args[11],
          a->data.pap->args[12], a->data.pap->args[13], a->data.pap->args[14]));
  }
  }
  fprintf(stderr, "IMPOSSIBLE: reached end of 'call' \n");
  exit(1);
}
