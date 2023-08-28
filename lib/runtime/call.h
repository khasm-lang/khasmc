#pragma once
#ifndef KHASM_CALL
#define KHASM_CALL
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "obj.h"
#include "gc.h"
#include "type.h"
#define STRINGIFY(x) #x



#if defined(__unix__)

#define KHASM_ENTRY( name, size, ... )					\
  asm (									\
       "  .text\n"		                                        \
       "  .globl " STRINGIFY(name) "\n"					\
    "  .quad " STRINGIFY(size) "\n"\
    STRINGIFY(name) ":\n"						\
    "  jmp " STRINGIFY(name) "_impl\n"					\
       );								\
  extern kha_obj *name (__VA_ARGS__);					\
  kha_obj * name##_impl (__VA_ARGS__)

#elif defined(__APPLE__)

/* Apple does something wierd with their inline asm, more debugging required */
#define KHASM_ENTRY( name, size, ... )					\
  asm (									\
       "  .text\n"		                                        \
       "  .globl " STRINGIFY(name) "\n"					\
    "  .quad " STRINGIFY(size) "\n"\
    STRINGIFY(name) ":\n"						\
    "  jmp _" STRINGIFY(name) "_impl\n"					\
       );								\
  extern kha_obj *name (__VA_ARGS__);					\
  kha_obj * name##_impl (__VA_ARGS__)

#endif

#define GET_ARGS(var, name)			\
  u64 var = *((u64*) ((u64)(name) - 8) )

#if defined(__clang__)
 
 #define MUSTTAIL  [[clang::musttail]]

#elif defined(__GNUC__)

 #pragma message "Cannot ensure TCO on GNUC"
 #define MUSTTAIL

#elif defined(_MSC_VER)

 #pragma message "Cannot ensure TCO on MSVC"
 #define MUSTTAIL

#else

 #pragma message "Cannot ensure TCO on whatever C compiler you're using"

#endif

kha_obj * add_arg(kha_obj *a, kha_obj *b);

kha_obj *reconcile(u64 arg, kha_obj *pap, kha_obj *ret);

kha_obj *call(kha_obj *a);

#endif
