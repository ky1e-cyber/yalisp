#if !defined(H_PARSE)
#define H_PARSE

#include <stdbool.h>
#include <stdio.h>
#include "arena.h"
#include "array.h"
#include "token.h"

typedef struct {
  FILE* file_desc;
  int current_char;
  size_t line_no;
  size_t line_pos;
  bool eof;
} lexer_state_t;

typedef enum {
  PT_TOPLEVEL,
  PT_LIST,
  PT_QUOTED,
  PT_ATOM,
  PT_LITERAL,
  PT_ERROR
} pt_node_kind_t;

typedef union {
  struct parse_tree_t_* as_subtree;
  array_ptr_t /* [parse_tree_t*] */ as_list;
  token_t as_token;
} pt_value_t;

typedef struct parse_tree_t_ {
  pt_node_kind_t kind;
  pt_value_t value;
  loc_t loc;
  const char* err_msg;
} parse_tree_t;

typedef struct {
  token_t current_token;
  bool eos;
} parser_state_t;

void fdump_tokens(FILE* stream,
                  const char* const src_path,
                  arena_ptr_t str_arena);

void fdump_parse_tree(FILE* stream,
                      const char* const src_path,
                      arena_ptr_t str_arena,
                      arena_ptr_t pt_arena);

parse_tree_t* parse(const char* const path,
                    arena_ptr_t str_arena,
                    arena_ptr_t pt_arena);

#endif
