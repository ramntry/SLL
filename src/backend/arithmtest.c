#include "runtime.h"

enum {
  Z_,
  S_,
  N_,
  F_,
  T_,
};

char const *const constructor_names[] = {
  "Z",
  "S",
  "N",
  "F",
  "T",
};

static inline Word *new_cell(size_t const object_size) {
  Word *cell = sll_free_cell[object_size];
  if (cell) {
    sll_free_cell[object_size] = (Word *)cell[0];
    return cell;
  }
  return sll_allocate_object(object_size);
}

static inline Object create_object_0(CtrId ctr_id) {
  Word *const cell = new_cell(0);
  cell[0] = SLL_make_header((Word)ctr_id, 0);
  return cell;
}

static inline Object create_object_1(CtrId ctr_id, Object const _1) {
  Word *const cell = new_cell(1);
  cell[0] = SLL_make_header((Word)ctr_id, 1);
  cell[1] = (Word)_1;
  return cell;
}

Object add_(Object ctr, Object y_) {
  Object result = NULL;
  switch ((CtrId)SLL_get_ctr_id(ctr[0])) {
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

int main() {
  Object const main_term = create_main_term();
  sll_print_value(main_term, constructor_names);
  sll_finalize();
  return 0;
}
