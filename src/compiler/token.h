#if !defined(H_TOKEN)
#define H_TOKEN

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct {
  size_t line_no;
  size_t line_pos;
} loc_t;

typedef enum {
  SPEC_DEFINE = 0,
  SPEC_LET,
  SPEC_PLUS,
  SPEC_MINUS,
  SPEC_STAR,
  SPEC_SLASH,
  SPEC_IF,
  SPEC_EQ,
  SPEC_NEQ,
  SPEC_LT,
  SPEC_LE,
  SPEC_GT,
  SPEC_GE,
  SPEC_NOT,
  SPEC_AND,
  SPEC_OR,
  SPEC_LAMBDA,
  SPEC_VECTOR,
  SPEC_COUNT__
} special_atom_t;

typedef enum {
  TK_INVALID = 0,
  TK_ROUND_PAREN_O,
  TK_ROUND_PAREN_C,
  TK_SQ_PAREN_O,
  TK_SQ_PAREN_C,
  TK_QUOTE,
  TK_BOOL_LITERAL,
  TK_INT_LITERAL,
  TK_STR_LITERAL,
  TK_ATOM,
  TK_SPECIAL_ATOM,
  TK_EOS,
  TK_COUNT__
} token_kind_t;

typedef struct {
  token_kind_t kind;
  loc_t loc;
  union {
    bool as_bool;
    int64_t as_i64;
    special_atom_t as_special_atom;
    char* as_cstr;
  } value;
  const char* err_msg;
} token_t;

#endif
