#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "types.h"
#include "khagm_obj.h"
#include "khagm_eval.h"

#define packed __attribute__ ((packed))


extern char * get_val_from_pointer(fptr p);

i32 khagm_obj_eq(khagm_obj * a, khagm_obj * b) {
  // TODO: make this less stupid and dumb lmao
  i64 x1 = (i64)(*a->data.FULL);
  i64 y1 = (i64)(*b->data.FULL);
  i32 x2 = (i32)(*(a->data.FULL + 8));
  i32 y2 = (i32)(*(b->data.FULL + 8));
  return (x1 == y1) && (x2 == y2);

}
khagm_obj * set_used(khagm_obj *a, i32 b) {
  a->used = b;
  return a;
}
khagm_obj * set_gc(khagm_obj *a, i8 b) {
  i32 mask = ((1 << 24) - 1) << 8;
  i32 first24 = a->used & mask;
  a->used = b & (first24 << 8);
  return a;
}

i32 get_used(khagm_obj *a) { return a->used; }

i8 get_gc(khagm_obj *a) { return a->used & (1 << 8) - 1; }

khagm_obj * khagm_obj_copy_thunk(khagm_obj * o) {
  khagm_obj * new = k_alloc(sizeof(khagm_obj));
  new->jump_point = o->jump_point;
  new->data.callable.args =
    k_alloc(sizeof(khagm_obj*) * (o->data.callable.argnum + 1));
  memcpy(new->data.callable.args,
	 o->data.callable.args,
	 sizeof(khagm_obj*) * (o->data.callable.argnum + 1));
  new->data.callable.argnum =
    o->data.callable.argnum;
  return set_used(new, get_used(o));
}


ptr_ll * head(void * p) {
  ptr_ll * h = k_alloc(sizeof(ptr_ll));
  h->p = p;
  h->next = NULL;
  return h;
}
ptr_ll * end(ptr_ll * p) {
  while (p->next) {
    p = p->next;
  }
  return p;
}

ptr_ll * add(ptr_ll * l, void *p) {
  ptr_ll * new = head(p);
  ptr_ll * e = end(l);
  e->next = new;
  return l;
}

int in(ptr_ll * l, void *p) {
  while (l) {
    if (l->p == p) {
      return 1;
    }
    l = l->next;
  }
  return 0;
}

void graph_thunk(khagm_obj * m, ptr_ll*p) {
  printf("thunk_%p;\n", m);
  printf("thunk_%p -> {", m);
  graphviz(m->data.callable.args[0], p);
  printf("}[arrowhead=odot];\n");
  for (int i = 1; i < m->data.callable.argnum + 1; i++) {
    printf("thunk_%p -> {", m);
    graphviz(m->data.callable.args[i], p);
    printf("};\n");
  }
}
void graph_seq(khagm_obj * m, ptr_ll*p) {
  printf("seq_%p -> seq1_%p;\n", m, m);
  printf("seq_%p -> seq2_%p;\n", m, m);
  printf("seq1_%p -> {", m);
  graphviz(m->data.seq.a[0], p);
  printf("};\nseq2_%p -> {", m);
  graphviz(m->data.seq.a[1], p);
  printf("};");
}
void graph_simpl(khagm_obj*m, ptr_ll*p) {
  printf("simpl_%p [label = \"int %ld float %f ptr %p\"];",
	 m, m->data.unboxed_int,
	 m->data.unboxed_float, m->data.val);
}

void graphviz(khagm_obj * m, ptr_ll*p) {
  if (in(p, m)) {
    return;
  }
  else {
    p = add(p, m);
  }
  if (m->jump_point == &handle_thunk) {
    graph_thunk(m, p);
  }
  else if (m->jump_point == &handle_seq) {
    graph_seq(m, p);
  }
  else if (m->jump_point == &handle_simpl){
    graph_simpl(m, p);
  }
  else {
    printf("unknown %p;\n", m);
  }
}

void as_graphviz(khagm_obj * m, int i) {
  ptr_ll * p = head(NULL);
  printf("\n\ndigraph %d {", i);
  graphviz(m, p);
  puts("}\n\n");
}
