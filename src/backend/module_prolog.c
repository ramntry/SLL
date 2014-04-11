#include "runtime.h"

#define CO_DECL(n) \
  static inline Object create_object_##n(CtrId const ctr_id
#define ARG(n) \
      , Object const _##n
#define CO_BEGIN(n) ) { \
    Word *const cell = sll_new_cell(n); \
    cell[0] = SLL_make_header((Word)ctr_id, n);
#define INIT(n) \
    cell[n] = (Word)_##n;
#define CO_END \
    return cell; \
  }

#define CT_DECL(n) \
  static inline Object create_thunk_##n(Applicator const app
#define CT_BEGIN(n) ) { \
    Word *const cell = sll_new_cell(n + 1); \
    cell[0] = SLL_make_header((Word)SllThunkId, n);
#define CT_END(n) \
    cell[n + 1] = (Word)app; \
    return cell; \
  }

#define REP_0(macro)
#define REP_1(macro) REP_0(macro) macro(1)
#define REP_2(macro) REP_1(macro) macro(2)
#define REP_3(macro) REP_2(macro) macro(3)
#define REP_4(macro) REP_3(macro) macro(4)
#define REP_5(macro) REP_4(macro) macro(5)
#define REP_6(macro) REP_5(macro) macro(6)
#define REP_7(macro) REP_6(macro) macro(7)
#define REP_8(macro) REP_7(macro) macro(8)
#define REP_9(macro) REP_8(macro) macro(9)
#define REP_10(macro) REP_9(macro) macro(10)
#define REP_11(macro) REP_10(macro) macro(11)
#define REP_12(macro) REP_11(macro) macro(12)
#define REP_13(macro) REP_12(macro) macro(13)
#define REP_14(macro) REP_13(macro) macro(14)
#define REP_15(macro) REP_14(macro) macro(15)

#define CREATE_OBJECT_DEF(n) \
  CO_DECL(n) REP_##n(ARG) CO_BEGIN(n) \
    REP_##n(INIT) \
  CO_END

#define CREATE_THUNK_DEF(n) \
  CT_DECL(n) REP_##n(ARG) CT_BEGIN(n) \
    REP_##n(INIT) \
  CT_END(n)

#define DEFS(n) \
  CREATE_OBJECT_DEF(n) \
  CREATE_THUNK_DEF(n)

DEFS(0)
DEFS(1)
DEFS(2)
DEFS(3)
DEFS(4)
DEFS(5)
DEFS(6)
DEFS(7)
DEFS(8)
DEFS(9)
DEFS(10)
DEFS(11)
DEFS(12)
DEFS(13)
DEFS(14)
DEFS(15)
