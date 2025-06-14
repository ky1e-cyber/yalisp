#if !defined(H_PARSE)
#define H_PARSE

#include <stdbool.h>
#include <stdio.h>
#include "program_tree.h"
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

program_tree_t* parse(const char* const path);

#endif
