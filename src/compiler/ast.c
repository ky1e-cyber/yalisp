#include <stdio.h>
#include "arena.h"
#include "ast.h"

static ast_t* ast_make_node(arena_ptr_t ast_arena,
                            loc_t loc,
                            ast_kind_t kind,
                            ast_value_t value,
                            const char* err_msg) {
  ast_t* node = (ast_t*)arena_alloc(ast_arena, sizeof(ast_t));

  node->loc = loc;
  node->kind = kind;
  node->value = value;
  node->err_msg = err_msg;

  return node;
}

ast_t* ast_make_error(arena_ptr_t ast_arena, loc_t loc, const char* err_msg) {
  return ast_make_node(ast_arena, loc, AST_ERROR, (ast_value_t){}, err_msg);
}

ast_t* ast_error_at(arena_ptr_t ast_arena,
                    arena_ptr_t str_arena,
                    loc_t loc,
                    const char* const err_fmt) {
  size_t len = strlen(err_fmt);

  char* err_msg = (char*)arena_alloc(str_arena, sizeof(char) * (len + 24));

  snprintf(err_msg, len + 24, err_fmt, loc.line_no, loc.line_pos);

  return ast_make_error(ast_arena, loc, err_msg);
}

ast_t* ast_make_toplevel(arena_ptr_t ast_arena, array_ptr_t toplvl_lst) {
  return ast_make_node(ast_arena, (loc_t){.line_no = 1, .line_pos = 1},
                       AST_TOPLEVEL,
                       (ast_value_t){.as_subtree_list = toplvl_lst}, NULL);
}

ast_t* ast_make_bool_literal(arena_ptr_t ast_arena, loc_t loc, bool value) {
  return ast_make_node(ast_arena, loc, AST_BOOL_LITERAL,
                       (ast_value_t){.as_bool = value}, NULL);
}

ast_t* ast_make_i64_literal(arena_ptr_t ast_arena, loc_t loc, int64_t value) {
  return ast_make_node(ast_arena, loc, AST_INT_LITERAL,
                       (ast_value_t){.as_i64 = value}, NULL);
}

ast_t* ast_make_string_literal(arena_ptr_t ast_arena,
                               loc_t loc,
                               const char* value) {
  return ast_make_node(ast_arena, loc, AST_STR_LITERAL,
                       (ast_value_t){.as_cstr = value}, NULL);
}

ast_t* ast_make_name(arena_ptr_t ast_arena, loc_t loc, name_id_t name_id) {
  return ast_make_node(ast_arena, loc, AST_NAME,
                       (ast_value_t){.as_name_id = name_id}, NULL);
}

ast_t* ast_make_binop(arena_ptr_t ast_arena,
                      loc_t loc,
                      binop_t op,
                      ast_t* lhs,
                      ast_t* rhs) {
  return ast_make_node(
      ast_arena, loc, AST_BINOP,
      (ast_value_t){.as_binop = {.op = op, .lhs = lhs, .rhs = rhs}}, NULL);
}

ast_t* ast_make_let(arena_ptr_t ast_arena,
                    loc_t loc,
                    bind_pair_t bind,
                    ast_t* expr) {
  return ast_make_node(
      ast_arena, loc, AST_LET,
      (ast_value_t){.as_let_form = {.bind = bind, .expr_subtree = expr}}, NULL);
}

ast_t* ast_make_if(arena_ptr_t ast_arena,
                   loc_t loc,
                   ast_t* cond_expr,
                   ast_t* t_branch,
                   ast_t* f_branch) {
  return ast_make_node(
      ast_arena, loc, AST_IF,
      (ast_value_t){.as_if_form = {.cond_subtree = cond_expr,
                                   .t_branch_subtree = t_branch,
                                   .f_branch_subtree = f_branch}},
      NULL);
}

ast_t* ast_make_lambda(arena_ptr_t ast_arena,
                       loc_t loc,
                       array_ptr_t /* [name_id_t] */ params,
                       array_ptr_t /* [name_id_t] */ captured,
                       ast_t* body) {
  return ast_make_node(ast_arena, loc, AST_LAMBDA,
                       (ast_value_t){.as_lambda = {.params = params,
                                                   .captured = captured,
                                                   .body_subtree = body}},
                       NULL);
}

ast_t* ast_make_call(arena_ptr_t ast_arena,
                     loc_t loc,
                     ast_t* fn,
                     array_ptr_t /* [ast_t*] */ args) {
  return ast_make_node(
      ast_arena, loc, AST_CALL,
      (ast_value_t){.as_call = {.fn_subtree = fn, .args_subtrees = args}},
      NULL);
}
