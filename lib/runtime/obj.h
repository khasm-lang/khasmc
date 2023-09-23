#ifndef KHASM_OBJ
#define KHASM_OBJ
#include "type.h"
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
  END,
  FREE
} packed kha_obj_typ;

typedef struct kha_obj {
  union {
    struct {
      kha_obj_typ tag;
      u64 gc: 56;
    };
    void *fatptr;
  };
  union {
    u64 kha_enum;

    struct kha_obj_adt {
      u64 tag;
      struct kha_obj **data;
    } *adt;
    
    struct kha_obj_pap {
      void *(*func)(void*);
      struct kha_obj **args;
      u64 argnum;
    } *pap;

    void *(*ptr)(void);

    i64 i;

    f64 f;

    struct kha_obj_tuple {
      u64 len;
      struct kha_obj ** tups;
    } *tuple;

    struct kha_obj_str {
      char * data;
      i64 len;
    } *str;
    
  }data;
} kha_obj;

kha_obj * make_ptr(kha_obj * p);
kha_obj * make_int(i64 i);

kha_obj * make_float(f64 f);

kha_obj *make_pap(u64 argnum, void *p, kha_obj **args);

kha_obj *make_tuple(u64 num, ...);
kha_obj *copy(kha_obj * a);
#endif
