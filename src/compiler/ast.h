#if !defined(H_AST)
#define H_AST

#include <stdbool.h>
#include <stdint.h>
#include "array.h"
#include "token.h"

typedef int name_id_t;

typedef struct ast_t__ ast_t;

typedef struct {
  name_id_t name_id;
  ast_t* value_subtree;
} bind_pair_t;

typedef struct {
  ast_t* fn_subtree;
  array_ptr_t /* [ast_t*] */ args_subtrees;
} ast_call_t;

typedef struct {
  bind_pair_t bind;
  ast_t* expr_subtree;
} ast_let_form_t;

typedef struct {
  ast_t* cond_subtree;
  ast_t* t_branch_subtree;
  ast_t* f_branch_subtree;
} ast_if_form_t;

typedef struct {
  array_ptr_t /* [name_id_t] */ params;
  array_ptr_t /* [name_id_t] */ captured;
  ast_t* body_subtree;
} ast_lambda_t;

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
  ast_t* lhs;
  ast_t* rhs;
} ast_binop_t;

typedef enum {
  AST_ERROR,
  AST_TOPLEVEL,
  AST_BOOL_LITERAL,
  AST_INT_LITERAL,
  AST_STR_LITERAL,
  AST_NAME,
  AST_BINOP,
  AST_LET,
  AST_IF,
  AST_LAMBDA,
  AST_CALL,
} ast_kind_t;

typedef union {
  array_ptr_t /* [ast_t*] */ as_subtree_list;
  bool as_bool;
  int64_t as_i64;
  const char* as_cstr;
  name_id_t as_name_id;
  ast_t* as_subtree;
  ast_binop_t as_binop;
  ast_call_t as_call;
  ast_let_form_t as_let_form;
  ast_if_form_t as_if_form;
  ast_lambda_t as_lambda;
} ast_value_t;

struct ast_t__ {
  ast_kind_t kind;
  loc_t loc;
  ast_value_t value;
  const char* err_msg;
};

ast_t* ast_make_error(arena_ptr_t ast_arena, loc_t loc, const char* err_msg);

ast_t* ast_error_at(arena_ptr_t ast_arena,
                    arena_ptr_t str_arena,
                    loc_t loc,
                    const char* const err_fmt);

ast_t* ast_make_toplevel(arena_ptr_t ast_arena, array_ptr_t toplvl_lst);

ast_t* ast_make_bool_literal(arena_ptr_t ast_arena, loc_t loc, bool value);

ast_t* ast_make_i64_literal(arena_ptr_t ast_arena, loc_t loc, int64_t value);

ast_t* ast_make_string_literal(arena_ptr_t ast_arena,
                               loc_t loc,
                               const char* value);

ast_t* ast_make_name(arena_ptr_t ast_arena, loc_t loc, name_id_t name_id);

ast_t* ast_make_binop(arena_ptr_t ast_arena,
                      loc_t loc,
                      binop_t op,
                      ast_t* lhs,
                      ast_t* rhs);

ast_t* ast_make_let(arena_ptr_t ast_arena,
                    loc_t loc,
                    bind_pair_t bind,
                    ast_t* expr);

ast_t* ast_make_if(arena_ptr_t ast_arena,
                   loc_t loc,
                   ast_t* cond_expr,
                   ast_t* t_branch,
                   ast_t* f_branch);

ast_t* ast_make_lambda(arena_ptr_t ast_arena,
                       loc_t loc,
                       array_ptr_t /* [name_id_t] */ params,
                       array_ptr_t /* [name_id_t] */ captured,
                       ast_t* body);

ast_t* ast_make_call(arena_ptr_t ast_arena,
                     loc_t loc,
                     ast_t* fn,
                     array_ptr_t /* [ast_t*] */ args);

#endif
