#include <stdio.h>
#include "arena.h"
#include "program_tree.h"

static program_tree_t* pt_make_node(arena_ptr_t pt_arena,
                                    loc_t loc,
                                    pt_kind_t kind,
                                    pt_value_t value,
                                    const char* err_msg) {
  program_tree_t* node =
      (program_tree_t*)arena_alloc(pt_arena, sizeof(program_tree_t));

  node->loc = loc;
  node->kind = kind;
  node->value = value;
  node->err_msg = err_msg;

  return node;
}

program_tree_t* pt_make_error(arena_ptr_t pt_arena,
                              loc_t loc,
                              const char* err_msg) {
  return pt_make_node(pt_arena, loc, PT_ERROR, (pt_value_t){}, err_msg);
}

program_tree_t* pt_error_at(arena_ptr_t pt_arena,
                            arena_ptr_t str_arena,
                            loc_t loc,
                            const char* const err_fmt) {
  size_t len = strlen(err_fmt);

  char* err_msg = (char*)arena_alloc(str_arena, sizeof(char) * (len + 24));

  snprintf(err_msg, len + 24, err_fmt, loc.line_no, loc.line_pos);

  return pt_make_error(pt_arena, loc, err_msg);
}

program_tree_t* pt_make_toplevel(arena_ptr_t pt_arena, array_ptr_t toplvl_lst) {
  return pt_make_node(pt_arena, (loc_t){.line_no = 1, .line_pos = 1},
                      PT_TOPLEVEL, (pt_value_t){.as_subtree_list = toplvl_lst},
                      NULL);
}

program_tree_t* pt_make_bool_literal(arena_ptr_t pt_arena,
                                     loc_t loc,
                                     bool value) {
  return pt_make_node(pt_arena, loc, PT_BOOL_LITERAL,
                      (pt_value_t){.as_bool = value}, NULL);
}

program_tree_t* pt_make_i64_literal(arena_ptr_t pt_arena,
                                    loc_t loc,
                                    int64_t value) {
  return pt_make_node(pt_arena, loc, PT_INT_LITERAL,
                      (pt_value_t){.as_i64 = value}, NULL);
}

program_tree_t* pt_make_string_literal(arena_ptr_t pt_arena,
                                       loc_t loc,
                                       const char* value) {
  return pt_make_node(pt_arena, loc, PT_STR_LITERAL,
                      (pt_value_t){.as_cstr = value}, NULL);
}

program_tree_t* pt_make_name(arena_ptr_t pt_arena,
                             loc_t loc,
                             name_id_t name_id) {
  return pt_make_node(pt_arena, loc, PT_NAME,
                      (pt_value_t){.as_name_id = name_id}, NULL);
}

program_tree_t* pt_make_binop(arena_ptr_t pt_arena,
                              loc_t loc,
                              binop_t op,
                              program_tree_t* lhs,
                              program_tree_t* rhs) {
  return pt_make_node(
      pt_arena, loc, PT_BINOP,
      (pt_value_t){.as_binop = {.op = op, .lhs = lhs, .rhs = rhs}}, NULL);
}

program_tree_t* pt_make_let(arena_ptr_t pt_arena,
                            loc_t loc,
                            bind_pair_t bind,
                            program_tree_t* expr) {
  return pt_make_node(
      pt_arena, loc, PT_LET,
      (pt_value_t){.as_let_form = {.bind = bind, .expr_subtree = expr}}, NULL);
}

program_tree_t* pt_make_if(arena_ptr_t pt_arena,
                           loc_t loc,
                           program_tree_t* cond_expr,
                           program_tree_t* t_branch,
                           program_tree_t* f_branch) {
  return pt_make_node(
      pt_arena, loc, PT_IF,
      (pt_value_t){.as_if_form = {.cond_subtree = cond_expr,
                                  .t_branch_subtree = t_branch,
                                  .f_branch_subtree = f_branch}},
      NULL);
}

program_tree_t* pt_make_lambda(arena_ptr_t pt_arena,
                               loc_t loc,
                               array_ptr_t /* [name_id_t] */ params,
                               array_ptr_t /* [name_id_t] */ captured,
                               program_tree_t* body) {
  return pt_make_node(pt_arena, loc, PT_LAMBDA,
                      (pt_value_t){.as_lambda = {.params = params,
                                                 .captured = captured,
                                                 .body_subtree = body}},
                      NULL);
}

program_tree_t* pt_make_call(arena_ptr_t pt_arena,
                             loc_t loc,
                             program_tree_t* fn,
                             array_ptr_t /* [ast_t*] */ args) {
  return pt_make_node(
      pt_arena, loc, PT_CALL,
      (pt_value_t){.as_call = {.fn_subtree = fn, .args_subtrees = args}}, NULL);
}

static void fpprint_ast_seq(FILE* stream,
                            array_ptr_t /* [ast_t*] */ lst,
                            char delim) {
  size_t seq_sz = array_size(lst);
  for (size_t i = 0; i < seq_sz - 1; i++) {
    fpprint_pt(stream, array_data(program_tree_t*, lst)[i]);
    fprintf(stream, "%c", delim);
  }

  if (seq_sz > 0)
    fpprint_pt(stream, array_data(program_tree_t*, lst)[seq_sz - 1]);
}

static void fpprint_ast_if(FILE* stream, pt_if_form_t if_form) {
  fprintf(stream, "if ");
  fpprint_pt(stream, if_form.cond_subtree);
  fprintf(stream, " then ");
  fpprint_pt(stream, if_form.t_branch_subtree);
  fprintf(stream, " else ");
  fpprint_pt(stream, if_form.f_branch_subtree);
}

static void fpprint_ast_call(FILE* stream, pt_call_t call) {
  fprintf(stream, "(");
  fpprint_pt(stream, call.fn_subtree);
  fprintf(stream, " ");
  fpprint_ast_seq(stream, call.args_subtrees, ' ');
  fprintf(stream, ")");
}

static const char* binop_repr(binop_t op) {
  static const char* const sum = "+";
  static const char* const sub = "-";
  static const char* const mul = "*";
  static const char* const div = "/";
  static const char* const le = "<=";
  static const char* const lt = "<";
  static const char* const ge = ">=";
  static const char* const gt = ">";
  static const char* const eq = "=";
  static const char* const neq = "/=";

  switch (op) {
    case BINOP_SUM:
      return sum;
    case BINOP_SUB:
      return sub;
    case BINOP_MUL:
      return mul;
    case BINOP_DIV:
      return div;
    case BINOP_LE:
      return le;
    case BINOP_LT:
      return lt;
    case BINOP_GE:
      return ge;
    case BINOP_GT:
      return gt;
    case BINOP_EQ:
      return eq;
    case BINOP_NEQ:
      return neq;
    case BINOP_COUNT__:
      assert(false);
      m_unreachable;
  }
}

static void fpprint_ast_binop(FILE* stream, pt_binop_t binop) {
  fprintf(stream, "(OP(%s) ", binop_repr(binop.op));
  fpprint_pt(stream, binop.lhs);
  fprintf(stream, " ");
  fpprint_pt(stream, binop.rhs);
  fprintf(stream, ")");
}

static void fpprint_ast_let(FILE* stream, pt_let_form_t let) {
  fprintf(stream, "let $var%d := ", let.bind.name_id);
  fpprint_pt(stream, let.bind.value_subtree);
  fprintf(stream, " in ");
  fpprint_pt(stream, let.expr_subtree);
}

static void fpprint_ast_lambda(FILE* stream, pt_lambda_t lambda) {
  fprintf(stream, "[");
  size_t captured_sz = array_size(lambda.captured);
  for (size_t i = 0; i < captured_sz - 1; i++) {
    fprintf(stream, "$var%d ", array_data(name_id_t, lambda.captured)[i]);
  }
  if (captured_sz > 0)
    fprintf(stream, "$var%d",
            array_data(name_id_t, lambda.captured)[captured_sz - 1]);
  fprintf(stream, "]");
  fprintf(stream, "\\");
  array_ptr_t params = lambda.params;
  size_t params_sz = array_size(params);

  for (size_t i = 0; i < params_sz; i++) {
    fprintf(stream, "$var%d ", array_data(name_id_t, params)[i]);
  }

  fprintf(stream, "-> ");
  fpprint_pt(stream, lambda.body_subtree);
}

void fpprint_pt(FILE* stream, program_tree_t* pt) {
  switch (pt->kind) {
    case PT_ERROR:
      fprintf(stream, "ERROR(%s)", pt->err_msg);
      break;
    case PT_TOPLEVEL:
      fpprint_ast_seq(stream, pt->value.as_subtree_list, '\n');
      fprintf(stream, "\n");
      break;
    case PT_BOOL_LITERAL:
      fprintf(stream, "BOOL(%s)", pt->value.as_bool ? "true" : "false");
      break;
    case PT_INT_LITERAL:
      fprintf(stream, "INT(%lli)", pt->value.as_i64);
      break;
    case PT_STR_LITERAL:
      fprintf(stream, "STRING(%s)", pt->value.as_cstr);
      break;
    case PT_NAME:
      fprintf(stream, "$var%d", pt->value.as_name_id);
      break;
    case PT_BINOP:
      fpprint_ast_binop(stream, pt->value.as_binop);
      break;
    case PT_IF:
      fpprint_ast_if(stream, pt->value.as_if_form);
      break;
    case PT_CALL:
      fpprint_ast_call(stream, pt->value.as_call);
      break;
    case PT_LET:
      fpprint_ast_let(stream, pt->value.as_let_form);
      break;
    case PT_LAMBDA:
      fpprint_ast_lambda(stream, pt->value.as_lambda);
      break;
  }
}
