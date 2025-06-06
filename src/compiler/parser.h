#if !defined(H_PARSE)
#define H_PARSE

#include <stdbool.h>
#include <stdio.h>
#include "arena.h"
#include "ast.h"
#include "token.h"

typedef struct {
  FILE* file_desc;
  int current_char;
  size_t line_no;
  size_t line_pos;
  bool eof;
} lexer_state_t;

typedef struct {
  token_t current_token;
  bool eos;
} parser_state_t;

void fdump_tokens(FILE* stream,
                  const char* const src_path,
                  arena_ptr_t str_arena);

void fdump_ast(FILE* stream,
               const char* const src_path,
               arena_ptr_t ast_arena,
               arena_ptr_t str_arena);

ast_t* parse(const char* const path,
             arena_ptr_t ast_arena,
             arena_ptr_t str_arena);

#endif
