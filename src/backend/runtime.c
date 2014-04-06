#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "runtime.h"

#define SLL_BLOCK_SIZE 2520
#define SLL_MAX_CTR_NAME_LEN 63

enum GCColor {
  SllWhite = 0,
  SllBlack = 1
};

enum Lexeme {
  SllEof = -1,
  SllCtrName = -2
};

struct Block {
  struct Block *next;
  Word mem[SLL_BLOCK_SIZE];
};

struct RootsBlock *sll_roots;
Word *sll_free_cell[SLL_MAX_OBJECT_SIZE];
struct Block *sll_heap[SLL_MAX_OBJECT_SIZE];
static size_t heap_size;
static int next_lexeme;
static char ctr_name_buf[SLL_MAX_CTR_NAME_LEN + 1];

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

static int lex_next(int skip_newline) {
  if (next_lexeme) {
    int const result = next_lexeme;
    next_lexeme = 0;
    return result;
  }
  int next_char = 0;
  do next_char = getchar();
  while (isspace(next_char) && (skip_newline || next_char != '\n'));
  if (next_char == '\n')
    return SllEof;
  switch (next_char) {
    case '(':
    case ')':
    case ',':
      return next_char;
    case EOF:
      return SllEof;
  }
  if (!isupper(next_char))
    sll_fatal_error("Unexpected symbol in standard input");
  int i = 0;
  for (; isalnum(next_char); ++i) {
    ctr_name_buf[i] = next_char;
    next_char = getchar();
  }
  ungetc(next_char, stdin);
  ctr_name_buf[i] = 0;
  return SllCtrName;
}

static inline int lex_look(int skip_newline) {
  if (next_lexeme)
    return next_lexeme;
  return next_lexeme = lex_next(skip_newline);
}

static inline void lex_take(int lexeme) {
  if (lexeme != lex_next(1))
    sll_fatal_error("Unexpected lexeme in standard input");
}

static inline int string_comp(void const *const lhs, void const *const rhs) {
  return strcmp(*(char const *const *)lhs, *(char const *const *)rhs);
}

static Object parse_value(char const *const *ctr_names, size_t numof_ctrs, int skip_newline) {
  static char const *const key = ctr_name_buf;
  lex_take(SllCtrName);
  CtrId const ctr_id = (char const *const *)
    bsearch(&key, ctr_names, numof_ctrs, sizeof(char *), string_comp) - ctr_names;
  if (ctr_id >= numof_ctrs)
    sll_fatal_error("Unexpected constructor name");
  size_t numof_args = 0;
  struct {
    struct RootsBlock header;
    Object args[SLL_MAX_OBJECT_SIZE];
  } m = { { sll_roots, SLL_MAX_OBJECT_SIZE } };
  sll_roots = &m.header;
  if (lex_look(skip_newline) == '(') {
    lex_take('(');
    if (lex_look(1) == SllCtrName) {
      m.args[numof_args++] = parse_value(ctr_names, numof_ctrs, 1);
      while (lex_look(1) == ',') {
        lex_take(',');
        m.args[numof_args++] = parse_value(ctr_names, numof_ctrs, 1);
      }
    }
    lex_take(')');
  }
  Word *const cell = new_cell(numof_args);
  cell[0] = SLL_make_header((Word)ctr_id, numof_args);
  for (size_t i = 0; i < numof_args; ++i)
    cell[i + 1] = (Word)m.args[i];
  sll_roots = m.header.next;
  return cell;
}

Object sll_read_value(char const *vname, char const *const *ctr_names, size_t numof_ctrs) {
  printf("%s = ", vname);
  next_lexeme = 0;
  return parse_value(ctr_names, numof_ctrs, 0);
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
