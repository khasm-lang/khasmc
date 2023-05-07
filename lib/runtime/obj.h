#ifndef KHASM_OBJ
#define KHASM_OBJ
#include "type.h"
#define packed __attribute__((packed))
typedef struct kha_obj {
  enum {
    PartialApp,
    Adt,
    Enum,
  } tag;
  u32 gc;
  union {
    u64 kha_enum;

    struct kha_obj_adt {
      u64 tag;
      struct kha_obj **data;
    } *adt;
    
    struct kha_obj_pap {
      void *(*func)(void*);
      struct kha_obj **args;
    }*pap;

  }data;
} packed kha_obj;


#endif
