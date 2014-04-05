#include "runtime.h"

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

static inline Object create_object_2(CtrId ctr_id, Object const _1, Object const _2) {
  Word *const cell = new_cell(2);
  cell[0] = SLL_make_header((Word)ctr_id, 2);
  cell[1] = (Word)_1;
  cell[2] = (Word)_2;
  return cell;
}

static inline Object create_object_3(CtrId ctr_id, Object const _1, Object const _2, Object const _3) {
  Word *const cell = new_cell(3);
  cell[0] = SLL_make_header((Word)ctr_id, 3);
  cell[1] = (Word)_1;
  cell[2] = (Word)_2;
  cell[3] = (Word)_3;
  return cell;
}
