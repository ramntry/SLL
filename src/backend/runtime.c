#include <stdlib.h>
#include <stdio.h>

#include "runtime.h"

#define SLL_BLOCK_SIZE 2520

enum GCColor {
  SllWhite = 0,
  SllBlack = 1
};

struct Block {
  struct Block *next;
  Word mem[SLL_BLOCK_SIZE];
};

struct RootsBlock *sll_roots;
Word *sll_free_cell[SLL_MAX_OBJECT_SIZE];
struct Block *sll_heap[SLL_MAX_OBJECT_SIZE];
static size_t heap_size;

void sll_fatal_error(char const *message) {
  fprintf(stderr, "SLL Fatal Error: %s\n", message);
  exit(EXIT_FAILURE);
}

static inline void sll_gc_dfs(Word *const cell) {
  if (SLL_get_color(cell[0]) == SllBlack)
    return;
  cell[0] = SLL_set_color(cell[0], SllBlack);
  size_t const size = SLL_get_osize(cell[0]);
  for (size_t i = 1; i <= size; ++i)
    sll_gc_dfs((Word *)cell[i]);
}

void sll_gc_mark() {
  for (struct RootsBlock *block = sll_roots; block; block = block->next) {
    size_t const size = block->size;
    Object *objects = (Object *)(block + 1);
    for (size_t i = 0; i < size; ++i)
      if (objects[i])
        sll_gc_dfs((Word *)objects[i]);
  }
}

size_t sll_gc_sweep() {
  size_t live_heap_size = 0;
  for (size_t object_size = 0; object_size < SLL_MAX_OBJECT_SIZE; ++object_size) {
    sll_free_cell[object_size] = NULL;
    size_t const size_in_words = object_size + 1;
    size_t const block_size = (SLL_BLOCK_SIZE / size_in_words) * size_in_words;
    for (struct Block *block = sll_heap[object_size]; block; block = block->next)
      for (size_t i = 0; i < block_size; i += size_in_words)
        if (SLL_get_color(block->mem[i]) == SllWhite) {
          block->mem[i] = (Word)sll_free_cell[object_size];
          sll_free_cell[object_size] = &block->mem[i];
        } else {
          block->mem[i] = SLL_set_color(block->mem[i], SllWhite);
          live_heap_size += size_in_words;
        }
  }
  return live_heap_size;
}

void sll_gc_collect() {
  static size_t max_size = 0;
  if (heap_size <= max_size)
    return;
  sll_gc_mark();
  size_t const live_heap_size = sll_gc_sweep();
  size_t const next_max_size = (live_heap_size * 3) / 2;
  if (next_max_size > max_size)
    max_size = next_max_size;
}

Word *sll_allocate_object(size_t object_size) {
  sll_gc_collect();
  Word *const cell = sll_free_cell[object_size];
  if (cell) {
    sll_free_cell[object_size] = (Word *)cell[0];
    return cell;
  }

  struct Block *new_block = (struct Block *)malloc(sizeof(struct Block));
  if (!new_block)
    sll_fatal_error("Out of memory");
  new_block->next = sll_heap[object_size];
  sll_heap[object_size] = new_block;
  heap_size += SLL_BLOCK_SIZE;

  size_t const size_in_words = object_size + 1;
  size_t const block_size = (SLL_BLOCK_SIZE / size_in_words) * size_in_words;
  if (block_size > size_in_words)
    sll_free_cell[object_size] = &new_block->mem[size_in_words];
  for (size_t i = 2 * size_in_words; i < block_size; i += size_in_words)
    new_block->mem[i - size_in_words] = (Word)&new_block->mem[i];
  new_block->mem[block_size - size_in_words] = 0;
  return &new_block->mem[0];
}

void sll_print_value(Object value, char const *const *ctr_names) {
  CtrId const ctr_id = SLL_get_ctr_id(value[0]);
  size_t const size = SLL_get_osize(value[0]);
  printf("%s", ctr_names[ctr_id]);
  if (size)
    printf("(");
  for (size_t i = 1; i <= size; ++i) {
    if (i > 1)
      printf(", ");
    sll_print_value((Object)value[i], ctr_names);
  }
  if (size)
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
