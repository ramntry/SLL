#include <stdlib.h>
#include <stdio.h>

#include "runtime.h"

#define SLL_BLOCK_SIZE 2520

struct Block {
  struct Block *next;
  Word mem[SLL_BLOCK_SIZE];
};

struct Block *sll_heap[SLL_MAX_OBJECT_SIZE];
Word *sll_free_cell[SLL_MAX_OBJECT_SIZE];

void sll_fatal_error(char const *message) {
  fprintf(stderr, "SLL Fatal Error: %s\n", message);
  exit(EXIT_FAILURE);
}

Word *sll_allocate_object(size_t object_size) {
  struct Block *new_block = (struct Block *)malloc(sizeof(struct Block));
  if (!new_block)
    sll_fatal_error("Out of memory");
  new_block->next = sll_heap[object_size];
  sll_heap[object_size] = new_block;

  size_t const size_in_words = object_size + 1;
  Word *const result = &new_block->mem[0];
  Word *curr = result + size_in_words;
  sll_free_cell[object_size] = curr;
  Word *next = curr + size_in_words;
  Word *block_end = &new_block->mem[SLL_BLOCK_SIZE];
  while (next < block_end) {
    *curr = (Word)next;
    curr = next;
    next += size_in_words;
  }
  *curr = (Word)NULL;
  return result;
}

void sll_print_value(Object value, char const *const *ctr_names) {
  CtrId const ctr_id = SLL_get_ctr_id(value[0]);
  size_t const size = SLL_get_osize(value[0]);
  printf("%s(", ctr_names[ctr_id]);
  for (size_t i = 1; i <= size; ++i) {
    if (i > 1)
      printf(", ");
    sll_print_value((Object)value[i], ctr_names);
  }
  printf(")");
}

void sll_finalize() {
  for (size_t i = 0; i < SLL_MAX_OBJECT_SIZE; ++i) {
    struct Block *curr_block = sll_heap[i];
    while (curr_block) {
      struct Block *to_free = curr_block;
      curr_block = curr_block->next;
      free(to_free);
    }
  }
  printf("\n");
}
