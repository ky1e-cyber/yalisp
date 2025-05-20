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

void fdump_tokens(FILE* stream);

parse_tree_t* pt_make_toplevel_node(
    arena_ptr_t arena,
    loc_t loc,
    array_ptr_t /* [parse_tree_t*] */ toplevel_list);

parse_tree_t* pt_make_list_node(arena_ptr_t arena,
                                loc_t loc,
                                array_ptr_t /* [parse_tree_t*] */ list);

parse_tree_t* pt_make_quoted_node(arena_ptr_t arena,
                                  loc_t loc,
                                  parse_tree_t* subtree);

parse_tree_t* pt_make_atom_node(arena_ptr_t arena,
                                loc_t loc,
                                token_t atom_token);

parse_tree_t* pt_make_literal_node(arena_ptr_t arena,
                                   loc_t loc,
                                   token_t literal_token);

parse_tree_t* pt_make_error_node(arena_ptr_t arena,
                                 loc_t loc,
                                 const char* err_msg);

parse_tree_t* parse(const char* const path,
                    arena_ptr_t str_arena,
                    arena_ptr_t pt_arena);

#endif
