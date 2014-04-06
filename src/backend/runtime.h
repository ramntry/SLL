#ifndef __SLL_RUNTIME_H_
#define __SLL_RUNTIME_H_

#include <stddef.h>
#include <inttypes.h>

#define SLL_MAX_OBJECT_SIZE 15

typedef uintptr_t Word;
typedef Word *Object;
typedef Word const CtrId;

#define SLL_make_header(ctr_id, object_size) \
  (((object_size) << 18) | (((ctr_id) & 0xFFFFU) << 2))

#define SLL_get_ctr_id(header) \
  ((CtrId)(((header) >> 2) & 0xFFFFU))

#define SLL_get_osize(header) \
  ((size_t)((header) >> 18))

#define SLL_get_color(header) \
  ((unsigned char)((header) & 0x3U))

#define SLL_set_color(header, color) \
   ((header) | (((color) & 0x3U)))

struct RootsBlock {
  struct RootsBlock *const next;
  size_t const size;
};

extern struct RootsBlock *sll_roots;
extern Word *sll_free_cell[SLL_MAX_OBJECT_SIZE];

void sll_fatal_error(char const *message);
Word *sll_allocate_object(size_t object_size);
void sll_print_value(Object value, char const *const *ctr_names);
Object sll_read_value(char const *vname, char const *const *ctr_names, size_t numof_ctrs);
void sll_initialize();
void sll_finalize();

static inline Word *new_cell(size_t const object_size) {
  Word *const cell = sll_free_cell[object_size];
  if (cell) {
    sll_free_cell[object_size] = (Word *)cell[0];
    return cell;
  }
  return sll_allocate_object(object_size);
}

#endif  // __SLL_RUNTIME_H_
