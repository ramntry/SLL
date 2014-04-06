#include "runtime.h"

#define DECL(n) \
  static inline Object create_object_##n(CtrId ctr_id
#define ARG(n) \
      , Object const _##n
#define BEGIN(n) ) { \
    Word *const cell = new_cell(n); \
    cell[0] = SLL_make_header((Word)ctr_id, n);
#define INIT(n) \
    cell[n] = (Word)_##n;
#define END \
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
  DECL(n) REP_##n(ARG) BEGIN(n) \
    REP_##n(INIT) \
  END

CREATE_OBJECT_DEF(0)
CREATE_OBJECT_DEF(1)
CREATE_OBJECT_DEF(2)
CREATE_OBJECT_DEF(3)
CREATE_OBJECT_DEF(4)
CREATE_OBJECT_DEF(5)
CREATE_OBJECT_DEF(6)
CREATE_OBJECT_DEF(7)
CREATE_OBJECT_DEF(8)
CREATE_OBJECT_DEF(9)
CREATE_OBJECT_DEF(10)
CREATE_OBJECT_DEF(11)
CREATE_OBJECT_DEF(12)
CREATE_OBJECT_DEF(13)
CREATE_OBJECT_DEF(14)
CREATE_OBJECT_DEF(15)
