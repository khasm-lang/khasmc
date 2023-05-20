#include "call.h"

extern u64 used;

#if __STDC_VERSION__ > 202000UL
#ifdef __clang__
      [[clang::musttail]]
#endif
#ifdef __GNUC__
	__attribute__((musttail))
#endif
#ifdef  _MSC_VER
#pragma message "Cannot ensure TCO on MSVC"
#endif
#endif

kha_obj *pap_call(kha_obj *f) {
  kha_obj *(*func)(u64, kha_obj**)
    = f->data.pap->func;
  u64 argnum = f->data.pap->argnum;
  kha_obj * ret = func(f->data.pap->argnum,
		       f->data.pap->args);
  if (!ret) {
    return f;
  }
  else {
    if (ret->tag == PAP) {
      // add our args and recurse
      if (used >= argnum) {
        fprintf(stderr, "Number of used %ld cannot ex\
ceed number of args %ld\n", used, argnum);
	exit(1);
      }
      u64 new_argnum = (argnum - used);

      ret->data.pap->args =
	realloc(ret->data.pap->args,
		sizeof(kha_obj*)
		* (new_argnum + ret->data.pap->argnum));
      /* copy args over */
      memcpy(ret->data.pap->args
	     + ret->data.pap->argnum,
	     f->data.pap->args + used,
	     new_argnum * sizeof(kha_obj*));
      /* inc new refcounts */

      for(int i = used; i < new_argnum + used; i++) {
	ref(ret->data.pap->args[i]);
      }
      
      ret->data.pap->argnum += new_argnum;
      unref(f);
      return pap_call(ret);
    }
    else if (ret->tag == PTR) {
       if (used >= argnum) {
        fprintf(stderr, "Number of used %ld cannot ex\
ceed number of args %ld\n", used, argnum);
	exit(1);
      }
      u64 new_argnum = (argnum - used);
      
      kha_obj ** unused = malloc(sizeof(kha_obj *)
				 * (new_argnum));
      memcpy(unused,
	     f->data.pap->args + used,
	     new_argnum * (sizeof(kha_obj*)));
      kha_obj *new = make_pap(new_argnum,
			      ret->data.ptr,
			      unused);
      unref(ret);
      unref(f);
      return new;
    }
    else {
      /* we've got a concrete value, so return it */
      unref(f);
      return ret;
    }
  }
}

kha_obj *call(kha_obj *f, kha_obj *x) {
  if (f->tag == PAP) {
    u64 argnum = f->data.pap->argnum;
    f->data.pap->args =
      realloc(f->data.pap->args,
	      (argnum + 1) * sizeof(kha_obj *));
    f->data.pap->args[argnum] = ref(x);
    f->data.pap->argnum++;
    return pap_call(f);
  }
  else if (f->tag == PTR) {
    kha_obj ** args = malloc(sizeof(kha_obj *));
    args[0] = ref(x);
    kha_obj * pap = make_pap(1, f->data.ptr, args);
    unref(f);
    return pap_call(pap);
  }
  else {
    fprintf(stderr, "Cannot call non pointer\n");
    exit(1);
  }
  fprintf(stderr, "unreachable\n");
}
