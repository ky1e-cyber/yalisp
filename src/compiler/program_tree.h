#include "arena.h"
#if !defined(H_PROGRAM_TREE)
#define H_PROGRAM_TREE

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "array.h"
#include "token.h"

typedef struct program_tree_t__ program_tree_t;

typedef struct {
  int name_id;
  program_tree_t* value_subtree;
} bind_pair_t;

typedef struct {
  program_tree_t* fn_subtree;
  array_ptr_t /* [program_tree_t*] */ args_subtrees;
} pt_call_t;

typedef struct {
  bind_pair_t bind;
  program_tree_t* expr_subtree;
} pt_let_form_t;

typedef struct {
  program_tree_t* cond_subtree;
  program_tree_t* t_branch_subtree;
  program_tree_t* f_branch_subtree;
} pt_if_form_t;

typedef struct {
  int id;
  array_ptr_t /* [name_id_t] */ params;
  array_ptr_t /* [name_id_t] */ captured;
  program_tree_t* body_subtree;
} pt_lambda_t;

typedef enum {
  BINOP_SUM = 0,
  BINOP_SUB,
  BINOP_MUL,
  BINOP_DIV,
  BINOP_LE,
  BINOP_LT,
  BINOP_GE,
  BINOP_GT,
  BINOP_EQ,
  BINOP_NEQ,
  BINOP_COUNT__
} binop_t;

typedef struct {
  binop_t op;
  program_tree_t* lhs;
  program_tree_t* rhs;
} pt_binop_t;

typedef enum {
  PT_ERROR,
  PT_TOPLEVEL,
  PT_BOOL_LITERAL,
  PT_INT_LITERAL,
  PT_STR_LITERAL,
  PT_NAME,
  PT_BINOP,
  PT_LET,
  PT_IF,
  PT_LAMBDA,
  PT_CALL,
  PT_GLOBAL_SYMBOL,
  PT_VECTOR,
} pt_kind_t;

typedef union {
  array_ptr_t /* [program_tree_t*] */ as_subtree_list;
  bool as_bool;
  int64_t as_i64;
  const char* as_cstr;
  int as_name_id;
  program_tree_t* as_subtree;
  pt_binop_t as_binop;
  pt_call_t as_call;
  pt_let_form_t as_let_form;
  pt_if_form_t as_if_form;
  pt_lambda_t as_lambda;
} pt_value_t;

struct program_tree_t__ {
  pt_kind_t kind;
  loc_t loc;
  pt_value_t value;
  const char* err_msg;
};

program_tree_t* pt_make_error(arena_ptr_t pt_arena,
                              loc_t loc,
                              const char* err_msg);

program_tree_t* pt_error_at(arena_ptr_t pt_arena,
                            arena_ptr_t str_arena,
                            loc_t loc,
                            const char* const err_fmt);

program_tree_t* pt_make_toplevel(arena_ptr_t pt_arena, array_ptr_t toplvl_lst);

program_tree_t* pt_make_bool_literal(arena_ptr_t pt_arena,
                                     loc_t loc,
                                     bool value);

program_tree_t* pt_make_i64_literal(arena_ptr_t pt_arena,
                                    loc_t loc,
                                    int64_t value);

program_tree_t* pt_make_string_literal(arena_ptr_t pt_arena,
                                       loc_t loc,
                                       const char* value);

program_tree_t* pt_make_name(arena_ptr_t pt_arena,
                             loc_t loc,
                             int name_id);

program_tree_t* pt_make_binop(arena_ptr_t pt_arena,
                              loc_t loc,
                              binop_t op,
                              program_tree_t* lhs,
                              program_tree_t* rhs);

program_tree_t* pt_make_let(arena_ptr_t pt_arena,
                            loc_t loc,
                            bind_pair_t bind,
                            program_tree_t* expr);

program_tree_t* pt_make_if(arena_ptr_t pt_arena,
                           loc_t loc,
                           program_tree_t* cond_expr,
                           program_tree_t* t_branch,
                           program_tree_t* f_branch);

program_tree_t* pt_make_lambda(arena_ptr_t pt_arena,
                               loc_t loc,
                               array_ptr_t /* [name_id_t] */ params,
                               array_ptr_t /* [name_id_t] */ captured,
                               program_tree_t* body);

program_tree_t* pt_make_call(arena_ptr_t pt_arena,
                             loc_t loc,
                             program_tree_t* fn,
                             array_ptr_t /* [program_tree_t*] */ args);

program_tree_t* pt_make_global(arena_ptr_t pt_arena,
                               loc_t loc,
                               const char* symbol);

program_tree_t* pt_make_vector(arena_ptr_t pt_arena,
                               loc_t loc,
                               array_ptr_t /* [program_tree_t*] */ elems);

void fpprint_pt(FILE* stream, program_tree_t* pt);

#endif
