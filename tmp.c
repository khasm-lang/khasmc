
#include <assert.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#ifndef KHASM_TYPE
#define KHASM_TYPE
#include <stdbool.h>
#include <stdint.h>
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef float f32;
typedef double f64;
typedef i64 *fptr;
#define atomic _Atomic
#endif
#ifndef KHASM_OBJ
#define KHASM_OBJ
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define packed __attribute__((packed))

typedef enum kha_obj_typ {
  ADT,
  PAP,
  ENUM,
  PTR,
  INT,
  FLOAT,
  STR,
  TUPLE,
  END
} packed kha_obj_typ;

typedef struct kha_obj {
  union {
    struct {
      kha_obj_typ tag;
      u64 gc : 56;
    };
    void *fatptr;
  };
  union {
    u64 kha_enum;

    struct kha_obj_adt {
      u64 tag;
      u64 num;
      struct kha_obj **data;
    } *adt;

    struct kha_obj_pap {
      void *(*func)(void *);
      struct kha_obj **args;
      u64 argnum;
    } *pap;

    void *(*ptr)(void);

    i64 i;

    f64 f;

    struct kha_obj_tuple {
      u64 len;
      struct kha_obj **tups;
    } *tuple;

    struct kha_obj_str {
      char *data;
      i64 len;
    } *str;

  } data;
} kha_obj;

kha_obj *make_ptr(kha_obj *p);
kha_obj *make_int(i64 i);

kha_obj *make_float(f64 f);

kha_obj *make_pap(u64 argnum, void *p, kha_obj **args);

kha_obj *make_tuple(u64 num, ...);
kha_obj *copy(kha_obj *a);
#endif
#ifndef KHASM_GC
#define KHASM_GC

kha_obj *new_kha_obj(kha_obj_typ t);

kha_obj *ref(kha_obj *);
void unref(kha_obj *);
void k_free(kha_obj *);
void k_thread_free(kha_obj *);
void free_worker(void);
void true_free(kha_obj *);
#endif
#pragma once
#ifndef KHASM_CALL
#define KHASM_CALL
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define STRINGIFY(x) #x

#define KHASM_ENTRY(name, size, ...)                                           \
  asm("  .text\n"                                                              \
      "  .globl " STRINGIFY(name) "\n"                                         \
                                  "  .quad " STRINGIFY(size) "\n" STRINGIFY(   \
                                      name) ":\n"                              \
                                            "  jmp " STRINGIFY(                \
                                                name) "_impl\n");              \
  extern kha_obj *name(__VA_ARGS__);                                           \
  kha_obj *name##_impl(__VA_ARGS__)

#define GET_ARGS(var, name) u64 var = *((u64 *)((u64)(name)-8))

#ifdef __clang__
#define MUSTTAIL [[clang::musttail]]
#endif
#ifdef __GNUC__
#pragma message "Cannot ensure TCO on GNUC"
#define MUSTTAIL
#endif
#ifdef _MSC_VER
#pragma message "Cannot ensure TCO on MSVC"
#define MUSTTAIL

#else
#define MUSTTAIL

#endif

kha_obj *add_arg(kha_obj *a, kha_obj *b);

kha_obj *reconcile(u64 arg, kha_obj *pap, kha_obj *ret);

kha_obj *call(kha_obj *a);

#endif
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

kha_obj *new_kha_obj(kha_obj_typ t) {
  kha_obj *a = malloc(sizeof(kha_obj));
  a->tag = t;
  a->gc = 0;
  return a;
}

inline kha_obj *ref(kha_obj *a) {
  if (!a) {
    fprintf(stderr, "can't ref null\n");
    exit(1);
  }
  a->gc += 1;
  return a;
}

inline void unref(kha_obj *a) {
  if (!a) {
    return;
  }
  a->gc -= 1;
  if (a->gc <= 0) {
    k_free(a);
  }
}

void k_free(kha_obj *a) { true_free(a); }

void thread_unref(kha_obj *a) {
  if (!a)
    return;
  a->gc -= 1;
  if (a->gc <= 0) {
    k_free(a);
  }
}

void true_free(kha_obj *a) {
  if (!a)
    return;
  switch (a->tag) {
  case INT:
  case FLOAT:
  case ENUM:
  case PTR:
    free(a);
    break;
  case PAP: {
    for (int i = 0; i < a->data.pap->argnum; i++) {
      thread_unref(a->data.pap->args[i]);
    }
    free(a->data.pap->args);
    free(a->data.pap);
    free(a);
    break;
  }
  case ADT: {
    for (int i = 0; i < a->data.pap->argnum; i++) {
      thread_unref(a->data.adt->data[i]);
    }
    free(a->data.adt->data);
    free(a->data.adt);
    free(a);
    break;
  }
  case TUPLE: {
    for (int i = 0; i < a->data.tuple->len; i++) {
      thread_unref(a->data.tuple->tups[i]);
    }
    free(a->data.tuple->tups);
    free(a->data.tuple);
    free(a);
    break;
  }
  case STR: {
    free(a->data.str->data);
    free(a->data.str);
    free(a);
    break;
  }
  default:
  case END: {
    fprintf(stderr, "UNREACHABLE???\n");
    exit(1);
  }
  }
}
#include <stdarg.h>
#include <stdlib.h>

kha_obj *make_raw_ptr(void *p) {
  kha_obj *k = new_kha_obj(PTR);
  k->data.ptr = p;
  k->gc = 1;
  return k;
}

kha_obj *make_ptr(kha_obj *p) {
  kha_obj *k = new_kha_obj(PTR);
  k->data.ptr = ref(p);
  k->gc = 1;
  return k;
}

kha_obj *make_int(i64 i) {
  kha_obj *k = new_kha_obj(INT);
  k->data.i = i;
  k->gc = 1;
  return k;
}

kha_obj *make_float(f64 f) {
  kha_obj *k = new_kha_obj(FLOAT);
  k->data.f = f;
  k->gc = 1;
  return k;
}

kha_obj *make_string(char *f) {
  kha_obj *k = new_kha_obj(STR);
  k->data.str = malloc(sizeof(struct kha_obj_str));
  k->data.str->data = strdup(f);
  k->data.str->len = strlen(f);
  k->gc = 1;
  return k;
}

kha_obj *make_pap(u64 argnum, void *p, kha_obj **args) {
  kha_obj *k = new_kha_obj(PAP);
  k->data.pap = malloc(sizeof(struct kha_obj_pap));
  k->data.pap->argnum = argnum;
  k->data.pap->args = args;
  k->data.pap->func = p;
  k->gc = 1;
  return k;
}

kha_obj *make_tuple(u64 num, ...) {

  va_list ap;
  va_start(ap, num);
  kha_obj **arr = malloc(sizeof(kha_obj *) * num);
  for (int i = 0; i < num; i++) {
    arr[i] = ref(va_arg(ap, kha_obj *));
  }
  va_end(ap);

  kha_obj *k = new_kha_obj(TUPLE);
  k->data.pap = malloc(sizeof(struct kha_obj_tuple));
  k->data.tuple->len = num;
  k->data.tuple->tups = arr;
  k->gc = 1;
  return k;
}

kha_obj *copy(kha_obj *a) {
  if (a->tag == PAP) {
    kha_obj *new = new_kha_obj(PAP);
    new->data.pap = malloc(sizeof(struct kha_obj_pap));
    new->data.pap->argnum = a->data.pap->argnum;
    new->data.pap->func = a->data.pap->func;
    new->data.pap->args = malloc(sizeof(kha_obj *) * a->data.pap->argnum);
    memcpy(new->data.pap->args, a->data.pap->args,
           sizeof(kha_obj *) * a->data.pap->argnum);
    for (int i = 0; i < a->data.pap->argnum; i++) {
      ref(new->data.pap->args[i]);
    }
    new->gc = 1;
    return new;
  } else {
    fprintf(stderr, "TODO: copy other stuff");
    exit(1);
  }
}

extern u64 used;

kha_obj *copy_pap(kha_obj *a) {
  if (a->tag != PAP) {
    fprintf(stderr, "Can't copy_pap non-pap\n");
    exit(1);
  }
  kha_obj **args = malloc(sizeof(kha_obj *) * (a->data.pap->argnum));
  memcpy(args, a->data.pap->args, sizeof(kha_obj *) * a->data.pap->argnum);
  for (int i = 0; i < a->data.pap->argnum; i++) {
    ref(args[i]);
  }
  kha_obj *ret = make_pap(a->data.pap->argnum, a->data.pap->func, args);
  return ret;
}

kha_obj *add_arg(kha_obj *a, kha_obj *b) {
  if (a->tag == PAP) {

    // copy pap

    a = copy_pap(a);

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
    // ret = copy_pap(ret);

    ret->data.pap->args =
        realloc(ret->data.pap->args,
                sizeof(kha_obj *) * (ret->data.pap->argnum + extra));

    for (int i = ret->data.pap->argnum; i < ret->data.pap->argnum + extra;
         i++) {
      ret->data.pap->args[i] = ref(pap->data.pap->args[i]);
    }

    unref(pap);
    return MUSTTAIL call(ret);
  } else if (ret->tag == PTR) {
    kha_obj **args = malloc(sizeof(kha_obj *) * (pap->data.pap->argnum - arg));

    for (int i = 0; i < (pap->data.pap->argnum - arg); i++) {
      args[i] = ref(pap->data.pap->args[i]);
    }

    kha_obj *new = make_pap(pap->data.pap->argnum - arg, ret->data.ptr, args);

    unref(ret);
    unref(pap);
    return MUSTTAIL call(new);
  } else {
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
      kha_obj *(*f)(kha_obj *, kha_obj *) = func;
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
      kha_obj *(*f)(kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) =
          func;
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
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) =
          func;
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
                        tmp_8, tmp_9, tmp_10, tmp_11);
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
                    kha_obj *, kha_obj *, kha_obj *, kha_obj *, kha_obj *) =
          func;
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
#include <stdio.h>
#include <stdlib.h>

extern u64 used;

KHASM_ENTRY(khasm_46_Stdlib_46_s_95_eq, 2, kha_obj *b, kha_obj *c) {
  kha_obj *ret;
  if (b->tag != c->tag) {
    ret = make_int(0);
  } else if (b->data.i != c->data.i) {
    ret = make_int(0);
  } else {
    ret = make_int(1);
  }
  unref(b);
  unref(c);
  return ret;
}
KHASM_ENTRY(khasm_46_Stdlib_46_iadd, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID ADDITION\n");
    exit(1);
  }
  kha_obj *ret = make_int(b->data.i + c->data.i);
  unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_46_Stdlib_46_isub, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID SUBTRACTION\n");
    exit(1);
  }
  kha_obj *ret = make_int(b->data.i - c->data.i);
  unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_46_Stdlib_46_imul, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID MULT\n");
    exit(1);
  }
  kha_obj *ret = make_int(b->data.i * c->data.i);
  unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_46_Stdlib_46_idiv, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != INT) {
    fprintf(stderr, "INVALID DIV\n");
    exit(1);
  }
  kha_obj *ret = make_int((i64)b->data.i / c->data.i);
  unref(b);
  unref(c);
  return ret;
}

// FLOAT STUFF
KHASM_ENTRY(khasm_46_Stdlib_46_fadd, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FADDITION\n");
    exit(1);
  }
  kha_obj *ret = make_int(b->data.f + c->data.f);
  unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_46_Stdlib_46_fsub, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FSUBTRACTION\n");
    exit(1);
  }
  kha_obj *ret = make_int(b->data.f - c->data.f);
  unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_46_Stdlib_46_fmul, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FMULT\n");
    exit(1);
  }
  kha_obj *ret = make_int(b->data.f * c->data.f);
  unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_46_Stdlib_46_fdiv, 2, kha_obj *b, kha_obj *c) {
  if (b->tag != c->tag || b->tag != FLOAT) {
    fprintf(stderr, "INVALID FDIV\n");
    exit(1);
  }
  kha_obj *ret = make_int((f64)b->data.f / c->data.f);
  unref(b);
  unref(c);
  return ret;
}

KHASM_ENTRY(khasm_46_Stdlib_46_print_95_int, 1, kha_obj *b) {
  if (b->tag != INT) {
    fprintf(stderr, "INVALID PRINT INT\n");
  }
  printf("%ld\n", b->data.i);
  unref(b);
  return make_tuple(0, NULL);
}

KHASM_ENTRY(khasm_46_Stdlib_46_print_95_str, 1, kha_obj *b) {
  if (b->tag != STR) {
    fprintf(stderr, "INVALID PRINT STR\n");
  }
  printf("%s\n", b->data.str->data);
  unref(b);
  return make_tuple(0, NULL);
}

KHASM_ENTRY(khasm_46__45__1, 2, kha_obj *t, kha_obj *b) {
  if (t->tag != TUPLE) {
    fprintf(stderr, "CAN'T TUPACC NONTUP\n");
  }
  if (b->tag != INT) {
    fprintf(stderr, "CAN'T TUPACC WITH NONINT\n");
  }
  kha_obj *tmp = ref(t->data.tuple->tups[b->data.i]);
  unref(t);
  unref(b);
  return tmp;
}

/* EXTERN 2 khasm_2 `int_add */
/* EXTERN 4 khasm_2 `int_sub */
/* EXTERN 6 khasm_2 `int_mul */
/* EXTERN 8 khasm_2 `int_div */
/* EXTERN 10 khasm_2 `float_add */
/* EXTERN 12 khasm_2 `float_sub */
/* EXTERN 14 khasm_2 `float_div */
/* EXTERN 16 khasm_2 `float_mul */
/* EXTERN 18 khasm_1 `print_int */
/* EXTERN 20 khasm_1 `print_str */
/* EXTERN 22 khasm_1 `print_float */
/* EXTERN 24 khasm_2 `s_eq */
extern kha_obj *khasm_46_Stdlib_46__61_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__61_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_0 = NULL, *IFELSETEMP = NULL;
  tmp_var_0 = make_raw_ptr(&khasm_46_Stdlib_46_s_95_eq);
  kha_obj *kha_return = ref(tmp_var_0);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_0);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__43_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__43_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_1 = NULL, *IFELSETEMP = NULL;
  tmp_var_1 = make_raw_ptr(&khasm_46_Stdlib_46_iadd);
  kha_obj *kha_return = ref(tmp_var_1);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_1);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__45_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__45_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_2 = NULL, *IFELSETEMP = NULL;
  tmp_var_2 = make_raw_ptr(&khasm_46_Stdlib_46_isub);
  kha_obj *kha_return = ref(tmp_var_2);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_2);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__47_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__47_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_3 = NULL, *IFELSETEMP = NULL;
  tmp_var_3 = make_raw_ptr(&khasm_46_Stdlib_46_idiv);
  kha_obj *kha_return = ref(tmp_var_3);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_3);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__42_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__42_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_4 = NULL, *IFELSETEMP = NULL;
  tmp_var_4 = make_raw_ptr(&khasm_46_Stdlib_46_imul);
  kha_obj *kha_return = ref(tmp_var_4);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_4);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__43__46_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__43__46_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_5 = NULL, *IFELSETEMP = NULL;
  tmp_var_5 = make_raw_ptr(&khasm_46_Stdlib_46_fadd);
  kha_obj *kha_return = ref(tmp_var_5);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_5);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__45__46_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__45__46_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_6 = NULL, *IFELSETEMP = NULL;
  tmp_var_6 = make_raw_ptr(&khasm_46_Stdlib_46_fsub);
  kha_obj *kha_return = ref(tmp_var_6);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_6);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__42__46_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__42__46_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_7 = NULL, *IFELSETEMP = NULL;
  tmp_var_7 = make_raw_ptr(&khasm_46_Stdlib_46_fmul);
  kha_obj *kha_return = ref(tmp_var_7);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_7);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__47__46_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__47__46_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_8 = NULL, *IFELSETEMP = NULL;
  tmp_var_8 = make_raw_ptr(&khasm_46_Stdlib_46_fdiv);
  kha_obj *kha_return = ref(tmp_var_8);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_8);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46_pipe(kha_obj *a_0, kha_obj *a_1);
KHASM_ENTRY(khasm_46_Stdlib_46_pipe, 2, kha_obj *a_0, kha_obj *a_1) {
  kha_obj *khasm_35 = ref(a_0), *khasm_36 = ref(a_1);
  kha_obj *tmp_var_11 = NULL, *tmp_var_10 = NULL, *tmp_var_9 = NULL,
          *IFELSETEMP = NULL;
  tmp_var_9 = ref(khasm_36);
  tmp_var_10 = ref(khasm_35);
  tmp_var_11 = add_arg(tmp_var_9, tmp_var_10);
  kha_obj *kha_return = ref(tmp_var_11);
  unref(khasm_35);
  unref(khasm_36);
  unref(IFELSETEMP);
  unref(tmp_var_9);
  unref(tmp_var_10);
  unref(tmp_var_11);
  unref(a_0);
  unref(a_1);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__124__62_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__124__62_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_12 = NULL, *IFELSETEMP = NULL;
  tmp_var_12 = make_raw_ptr(&khasm_46_Stdlib_46_pipe);
  kha_obj *kha_return = ref(tmp_var_12);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_12);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46_apply(kha_obj *a_0, kha_obj *a_1);
KHASM_ENTRY(khasm_46_Stdlib_46_apply, 2, kha_obj *a_0, kha_obj *a_1) {
  kha_obj *khasm_39 = ref(a_0), *khasm_40 = ref(a_1);
  kha_obj *tmp_var_15 = NULL, *tmp_var_14 = NULL, *tmp_var_13 = NULL,
          *IFELSETEMP = NULL;
  tmp_var_13 = ref(khasm_39);
  tmp_var_14 = ref(khasm_40);
  tmp_var_15 = add_arg(tmp_var_13, tmp_var_14);
  kha_obj *kha_return = ref(tmp_var_15);
  unref(khasm_39);
  unref(khasm_40);
  unref(IFELSETEMP);
  unref(tmp_var_13);
  unref(tmp_var_14);
  unref(tmp_var_15);
  unref(a_0);
  unref(a_1);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__36_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__36_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_16 = NULL, *IFELSETEMP = NULL;
  tmp_var_16 = make_raw_ptr(&khasm_46_Stdlib_46_apply);
  kha_obj *kha_return = ref(tmp_var_16);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_16);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46_compose(kha_obj *a_0, kha_obj *a_1,
                                           kha_obj *a_2);
KHASM_ENTRY(khasm_46_Stdlib_46_compose, 3, kha_obj *a_0, kha_obj *a_1,
            kha_obj *a_2) {
  kha_obj *khasm_43 = ref(a_0), *khasm_44 = ref(a_1), *khasm_45 = ref(a_2);
  kha_obj *tmp_var_21 = NULL, *tmp_var_20 = NULL, *tmp_var_19 = NULL,
          *tmp_var_18 = NULL, *tmp_var_17 = NULL, *IFELSETEMP = NULL;
  tmp_var_17 = ref(khasm_43);
  tmp_var_18 = ref(khasm_44);
  tmp_var_19 = ref(khasm_45);
  tmp_var_20 = add_arg(tmp_var_18, tmp_var_19);
  tmp_var_21 = add_arg(tmp_var_17, tmp_var_20);
  kha_obj *kha_return = ref(tmp_var_21);
  unref(khasm_43);
  unref(khasm_44);
  unref(khasm_45);
  unref(IFELSETEMP);
  unref(tmp_var_17);
  unref(tmp_var_18);
  unref(tmp_var_19);
  unref(tmp_var_20);
  unref(tmp_var_21);
  unref(a_0);
  unref(a_1);
  unref(a_2);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__37_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__37_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_22 = NULL, *IFELSETEMP = NULL;
  tmp_var_22 = make_raw_ptr(&khasm_46_Stdlib_46_compose);
  kha_obj *kha_return = ref(tmp_var_22);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_22);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46_rcompose(kha_obj *a_0, kha_obj *a_1,
                                            kha_obj *a_2);
KHASM_ENTRY(khasm_46_Stdlib_46_rcompose, 3, kha_obj *a_0, kha_obj *a_1,
            kha_obj *a_2) {
  kha_obj *khasm_48 = ref(a_0), *khasm_49 = ref(a_1), *khasm_50 = ref(a_2);
  kha_obj *tmp_var_27 = NULL, *tmp_var_26 = NULL, *tmp_var_25 = NULL,
          *tmp_var_24 = NULL, *tmp_var_23 = NULL, *IFELSETEMP = NULL;
  tmp_var_23 = ref(khasm_49);
  tmp_var_24 = ref(khasm_48);
  tmp_var_25 = ref(khasm_50);
  tmp_var_26 = add_arg(tmp_var_24, tmp_var_25);
  tmp_var_27 = add_arg(tmp_var_23, tmp_var_26);
  kha_obj *kha_return = ref(tmp_var_27);
  unref(khasm_48);
  unref(khasm_49);
  unref(khasm_50);
  unref(IFELSETEMP);
  unref(tmp_var_23);
  unref(tmp_var_24);
  unref(tmp_var_25);
  unref(tmp_var_26);
  unref(tmp_var_27);
  unref(a_0);
  unref(a_1);
  unref(a_2);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Stdlib_46__37__62_(void);
KHASM_ENTRY(khasm_46_Stdlib_46__37__62_, 0, void) {
  /*EMPTY*/ kha_obj *tmp_var_28 = NULL, *IFELSETEMP = NULL;
  tmp_var_28 = make_raw_ptr(&khasm_46_Stdlib_46_rcompose);
  kha_obj *kha_return = ref(tmp_var_28);
  /*EMPTY*/ unref(IFELSETEMP);
  unref(tmp_var_28);
  ;
  return kha_return;
}
extern kha_obj *khasm_46_Types_46_do_95_thing(kha_obj *a_0);
KHASM_ENTRY(khasm_46_Types_46_do_95_thing, 1, kha_obj *a_0) {
  kha_obj *khasm_53 = ref(a_0);
  kha_obj *tmp_var_31 = NULL, *tmp_var_30 = NULL, *tmp_var_29 = NULL,
          *IFELSETEMP = NULL;
  tmp_var_29 = make_raw_ptr(&khasm_46_Stdlib_46_print_95_str);
  tmp_var_30 = make_string("hi");
  tmp_var_31 = add_arg(tmp_var_29, tmp_var_30);
  kha_obj *kha_return = ref(tmp_var_31);
  unref(khasm_53);
  unref(IFELSETEMP);
  unref(tmp_var_29);
  unref(tmp_var_30);
  unref(tmp_var_31);
  unref(a_0);
  ;
  return kha_return;
}
extern kha_obj *main_____Khasm(kha_obj *a_0);
KHASM_ENTRY(main_____Khasm, 1, kha_obj *a_0) {
  kha_obj *khasm_54 = ref(a_0);
  kha_obj *tmp_var_34 = NULL, *tmp_var_33 = NULL, *tmp_var_32 = NULL,
          *IFELSETEMP = NULL;
  tmp_var_32 = make_raw_ptr(&khasm_46_Types_46_do_95_thing);
  tmp_var_33 = make_tuple(0);
  tmp_var_34 = add_arg(tmp_var_32, tmp_var_33);
  kha_obj *kha_return = ref(tmp_var_34);
  unref(khasm_54);
  unref(IFELSETEMP);
  unref(tmp_var_32);
  unref(tmp_var_33);
  unref(tmp_var_34);
  unref(a_0);
  ;
  return kha_return;
}

int main(void) {
  kha_obj *empty = make_tuple(0);
  kha_obj *ret = main_____Khasm(empty);
  if (ret->tag != TUPLE) {
    fprintf(stderr, "RETURN VALUE NOT TUPLE - TYPE SYSTEM INVALID\n");
  }
  unref(ret);
  return 0;
}
