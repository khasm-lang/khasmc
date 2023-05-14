#ifndef KHASM_OBJ
#define KHASM_OBJ
#include "type.h"
#define packed __attribute__((packed))

typedef enum kha_obj_typ {
  ADT,
  PAP,
  ENUM,
  PTR,
  INT,
  FLOAT,
  STR
} packed kha_obj_typ;

typedef struct kha_obj {
  kha_obj_typ tag;
  u64 gc: 56;
  union {
    u64 kha_enum;

    struct kha_obj_adt {
      u64 tag;
      u64 num;
      struct kha_obj **data;
    } *adt;
    
    struct kha_obj_pap {
      void *(*func)(void*);
      struct kha_obj **args;
      i64 argnum;
    } *pap;

    void *(*ptr)(void);

    i64 i;

    f64 f;

    struct kha_obj_str {
      char * data;
      i64 len;
    } *str;
    
  }data;
} packed kha_obj;


#endif
