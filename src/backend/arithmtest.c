#include "module_prolog.c"

enum {
  Z_,
  S_,
};

char const *const constructor_names[] = {
  "Z",
  "S",
};

Object add_(Object ctr, Object y_) {
  Object result = NULL;
  switch (SLL_get_ctr_id(ctr[0])) {
    case Z_: {
      result = y_;
      break;
  } case S_: {
      Object const gcall_add = add_((Object)ctr[1], y_);
      Object const ctr_S = create_object_1(S_, gcall_add);
      result = ctr_S;
      break;
  } default:
      sll_fatal_error("Inexhaustive pattern matching");
  }
  return result;
}

Object create_main_term() {
  Object const ctr_Z = create_object_0(Z_);
  Object const ctr_S = create_object_1(S_, ctr_Z);
  Object const ctr_S_1 = create_object_1(S_, ctr_S);
  Object const ctr_S_2 = create_object_1(S_, ctr_S_1);
  Object const ctr_Z_1 = create_object_0(Z_);
  Object const ctr_S_3 = create_object_1(S_, ctr_Z_1);
  Object const gcall_add = add_(ctr_S_2, ctr_S_3);
  return gcall_add;
}

#include "module_epilog.c"
