#include "gc.h"
#include "call.h"

extern u64 used;


kha_obj * copy_pap(kha_obj *a) {
  if (a->tag != PAP) {
    fprintf(stderr, "Can't copy_pap non-pap\n");
    exit(1);
  }
  kha_obj ** args = kha_alloc(sizeof(kha_obj*)
			   * (a->data.pap->argnum));
  memcpy(args, a->data.pap->args,
	 sizeof(kha_obj *) * a->data.pap->argnum);
  for(int i = 0; i < a->data.pap->argnum; i++) {
    ref(args[i]);
  }
  kha_obj * ret = make_pap(a->data.pap->argnum,
			   a->data.pap->func,
			   args);
  return ret;
}

kha_obj * add_arg(kha_obj *f, kha_obj *b) {
  if (f->tag == PAP) {

    // copy pap

    kha_obj * a = copy_pap(f);
    
    a->data.pap->argnum++;
    a->data.pap->args =
        realloc(a->data.pap->args,
		a->data.pap->argnum * sizeof(kha_obj *));
    a->data.pap->args[a->data.pap->argnum - 1] = ref(b);
    return call(a);
  } else if (f->tag == PTR) {
    kha_obj **args = kha_alloc(sizeof(kha_obj *));
    args[0] = ref(b);
    kha_obj *k = make_pap(1, f->data.ptr, args);
    unref(f);
    return call(k);
  } else {
    fprintf(stderr, "ERROR: Can't call non-ptr %d", f->tag);
    exit(1);
  }
}

kha_obj *reconcile(u64 arg, kha_obj *pap, kha_obj *ret) {
  u64 extra = pap->data.pap->argnum - arg;
  if (ret->tag == PAP) {
    //ret = copy_pap(ret);
    
    ret->data.pap->args = realloc(ret->data.pap->args,
				  sizeof(kha_obj *)
				  * (ret->data.pap->argnum + extra));
    
    for(int i = ret->data.pap->argnum; i < ret->data.pap->argnum + extra; i++) {
      ret->data.pap->args[i] = ref(pap->data.pap->args[i]);
    }
    
    unref(pap);
    return MUSTTAIL call(ret);
  }
  else if (ret->tag == PTR) {
    kha_obj **args = kha_alloc(sizeof(kha_obj *) * (pap->data.pap->argnum - arg));
    
    for (int i = 0; i < (pap->data.pap->argnum - arg); i++) {
      args[i] = ref(pap->data.pap->args[i]);
    }

    kha_obj * new = make_pap(pap->data.pap->argnum - arg, ret->data.ptr, args);
    
    unref(ret);
    unref(pap);
    return MUSTTAIL call(new);
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
      unref(a);
      return MUSTTAIL f();
    }
      
    case 1: {
      kha_obj *tmp_0;
      kha_obj *(*f)(kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      unref(a);
      return MUSTTAIL f(tmp_0);
    }
      
    case 2: {
      kha_obj *tmp_0, *tmp_1;
      kha_obj *(*f)(kha_obj *, kha_obj *)  = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1);
    }

    case 3: {
      kha_obj *tmp_0, *tmp_1, *tmp_2;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2);
    }

    case 4: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3);
    }

    case 5: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4);
    }

    case 6: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5);
    }

    case 7: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6);
    }

    case 8: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6, *tmp_7;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      tmp_7 = ref(a->data.pap->args[7]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7);
    }

    case 9: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6, *tmp_7,
	*tmp_8;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      tmp_7 = ref(a->data.pap->args[7]);
      tmp_8 = ref(a->data.pap->args[8]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7,
			tmp_8);
    }

    case 10: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6, *tmp_7,
	*tmp_8, *tmp_9;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      tmp_7 = ref(a->data.pap->args[7]);
      tmp_8 = ref(a->data.pap->args[8]);
      tmp_9 = ref(a->data.pap->args[9]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7,
			tmp_8, tmp_9);
    }

    case 11: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6, *tmp_7,
	*tmp_8, *tmp_9, *tmp_10;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      tmp_7 = ref(a->data.pap->args[7]);
      tmp_8 = ref(a->data.pap->args[8]);
      tmp_9 = ref(a->data.pap->args[9]);
      tmp_10 = ref(a->data.pap->args[10]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7,
			tmp_8, tmp_9, tmp_10);
    }

    case 12: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6, *tmp_7,
	*tmp_8, *tmp_9, *tmp_10, *tmp_11;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      tmp_7 = ref(a->data.pap->args[7]);
      tmp_8 = ref(a->data.pap->args[8]);
      tmp_9 = ref(a->data.pap->args[9]);
      tmp_10 = ref(a->data.pap->args[10]);
      tmp_11 = ref(a->data.pap->args[11]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7,
			tmp_8, tmp_9, tmp_10, tmp_11) ;
    }

    case 13: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6, *tmp_7,
	*tmp_8, *tmp_9, *tmp_10, *tmp_11, *tmp_12;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      tmp_7 = ref(a->data.pap->args[7]);
      tmp_8 = ref(a->data.pap->args[8]);
      tmp_9 = ref(a->data.pap->args[9]);
      tmp_10 = ref(a->data.pap->args[10]);
      tmp_11 = ref(a->data.pap->args[11]);
      tmp_12 = ref(a->data.pap->args[12]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7,
			tmp_8, tmp_9, tmp_10, tmp_11, tmp_12);
    }

    case 14: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6, *tmp_7,
	*tmp_8, *tmp_9, *tmp_10, *tmp_11, *tmp_12, *tmp_13;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      tmp_7 = ref(a->data.pap->args[7]);
      tmp_8 = ref(a->data.pap->args[8]);
      tmp_9 = ref(a->data.pap->args[9]);
      tmp_10 = ref(a->data.pap->args[10]);
      tmp_11 = ref(a->data.pap->args[11]);
      tmp_12 = ref(a->data.pap->args[12]);
      tmp_13 = ref(a->data.pap->args[13]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7,
			tmp_8, tmp_9, tmp_10, tmp_11, tmp_12, tmp_13);
    }

    case 15: {
      kha_obj *tmp_0, *tmp_1, *tmp_2, *tmp_3, *tmp_4, *tmp_5, *tmp_6, *tmp_7,
	*tmp_8, *tmp_9, *tmp_10, *tmp_11, *tmp_12, *tmp_13, *tmp_14;
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *,
		    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) = func;
      tmp_0 = ref(a->data.pap->args[0]);
      tmp_1 = ref(a->data.pap->args[1]);
      tmp_2 = ref(a->data.pap->args[2]);
      tmp_3 = ref(a->data.pap->args[3]);
      tmp_4 = ref(a->data.pap->args[4]);
      tmp_5 = ref(a->data.pap->args[5]);
      tmp_6 = ref(a->data.pap->args[6]);
      tmp_7 = ref(a->data.pap->args[7]);
      tmp_8 = ref(a->data.pap->args[8]);
      tmp_9 = ref(a->data.pap->args[9]);
      tmp_10 = ref(a->data.pap->args[10]);
      tmp_11 = ref(a->data.pap->args[11]);
      tmp_12 = ref(a->data.pap->args[12]);
      tmp_13 = ref(a->data.pap->args[13]);
      tmp_14 = ref(a->data.pap->args[14]);
      unref(a);
      return MUSTTAIL f(tmp_0, tmp_1, tmp_2, tmp_3, tmp_4, tmp_5, tmp_6, tmp_7,
			tmp_8, tmp_9, tmp_10, tmp_11, tmp_12, tmp_13, tmp_14);
    }}
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
