#ifndef __SLL_RUNTIME_H_
#define __SLL_RUNTIME_H_

#include <stddef.h>
#include <inttypes.h>

#define SLL_MAX_OBJECT_SIZE 15

#if   defined SLL_STRICT && !defined SLL_BYNAME && !defined SLL_BYNEED
# define SLL_HEAD_FORM(obj) obj
#elif defined SLL_BYNAME && !defined SLL_STRICT && !defined SLL_BYNEED
# define SLL_HEAD_FORM sll_head_form
#elif defined SLL_BYNEED && !defined SLL_STRICT && !defined SLL_BYNAME
# define SLL_HEAD_FORM sll_fast_head_form
#else
# error You must specify exactly one evaluation strategy: SLL_STRICT or SLL_BYNAME or SLL_BYNEED
#endif

typedef uintptr_t Word;
typedef Word *Object;
typedef Word CtrId;

enum BuiltinCtrIds {
  SllExternalCtrId = 0xFFFF,
  SllThunkId       = 0xFFFE,
  SllCachedThunkId = 0xFFFD
};

#define SLL_make_header(ctr_id, object_size) \
  (((object_size) << 18) | (((ctr_id) & 0xFFFFUL) << 2))

#define SLL_get_ctr_id(header) \
  ((CtrId)(((header) >> 2) & 0xFFFFUL))

#define SLL_get_osize(header) \
  ((size_t)((header) >> 18))

#define SLL_get_color(header) \
  ((unsigned char)((header) & 0x3UL))

#define SLL_set_color(header, color) \
   (((header) & ~0x3UL) | (((color) & 0x3UL)))

struct RootsBlock {
  struct RootsBlock *const next;
  size_t const size;
};

extern struct RootsBlock *sll_roots;
extern Word *sll_free_cell[SLL_MAX_OBJECT_SIZE];

void sll_fatal_error(char const *message);
Word *sll_allocate_object(size_t object_size);
void sll_print_value(Object const value, char const *const *ctr_names);
Object sll_read_value(char const *vname, char const *const *ctr_names, size_t numof_ctrs);
void sll_initialize();
void sll_finalize();

static inline Word *sll_new_cell(size_t const object_size) {
  Word *const cell = sll_free_cell[object_size];
  if (cell) {
    sll_free_cell[object_size] = (Word *)cell[0];
    return cell;
  }
  return sll_allocate_object(object_size);
}

typedef Object (*Applicator)(Object const obj);

static inline Object sll_head_form(Object const obj) {
  struct {
    struct RootsBlock header;
    Object obj;
  } m = { { sll_roots, 1 }, obj };
  sll_roots = &m.header;
  while (SLL_get_ctr_id(m.obj[0]) == SllThunkId)
    m.obj = ((Applicator)m.obj[SLL_get_osize(m.obj[0]) + 1])(m.obj);
  sll_roots = m.header.next;
  return m.obj;
}

static inline Object sll_fast_head_form(Object const obj) {
  switch (SLL_get_ctr_id(obj[0])) {
    case SllThunkId: {
      struct {
        struct RootsBlock header;
        Object head_form;
      } m = { { sll_roots, 1 } };
      sll_roots = &m.header;
      m.head_form = ((Applicator)obj[SLL_get_osize(obj[0]) + 1])(obj);
      m.head_form = sll_fast_head_form(m.head_form);
      obj[0] = SLL_make_header(SllCachedThunkId, 1);
      obj[1] = (Word)m.head_form;
      sll_roots = m.header.next;
      return m.head_form;
    }
    case SllCachedThunkId:
      return (Object)obj[1];
    default:
      return obj;
  }
}

#endif  // __SLL_RUNTIME_H_
