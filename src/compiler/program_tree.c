#include <assert.h>
#include <stdio.h>
#include "arena.h"
#include "array.h"
#include "macros.h"
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
                                       char* value) {
  return pt_make_node(pt_arena, loc, PT_STR_LITERAL,
                      (pt_value_t){.as_str_literal = {.id = -1, .cstr = value}},
                      NULL);
}

program_tree_t* pt_make_name(arena_ptr_t pt_arena, loc_t loc, int name_id) {
  return pt_make_node(pt_arena, loc, PT_NAME,
                      (pt_value_t){.as_name_id = name_id}, NULL);
}

program_tree_t* pt_make_uop(arena_ptr_t pt_arena,
                            loc_t loc,
                            uop_t op,
                            program_tree_t* operand) {
  return pt_make_node(pt_arena, loc, PT_UOP,
                      (pt_value_t){.as_uop = {.operand = operand, .op = op}},
                      NULL);
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
                      (pt_value_t){.as_lambda = {.id = -1,
                                                 .params = params,
                                                 .captured = captured,
                                                 .body_subtree = body}},
                      NULL);
}

program_tree_t* pt_make_call(arena_ptr_t pt_arena,
                             loc_t loc,
                             program_tree_t* fn,
                             array_ptr_t /* [program_tree_t*] */ args) {
  return pt_make_node(
      pt_arena, loc, PT_CALL,
      (pt_value_t){.as_call = {.fn_subtree = fn, .args_subtrees = args}}, NULL);
}

program_tree_t* pt_make_global(arena_ptr_t pt_arena, loc_t loc, char* symbol) {
  return pt_make_node(pt_arena, loc, PT_GLOBAL_SYMBOL,
                      (pt_value_t){.as_symbol = symbol}, NULL);
}

static void pprint_pt_seq(array_ptr_t /* [program_tree_t*] */ lst, char delim) {
  size_t seq_sz = array_size(lst);
  if (seq_sz > 0) {
    for (size_t i = 0; i < seq_sz - 1; i++) {
      pt_pprint(array_data(program_tree_t*, lst)[i]);
      printf("%c", delim);
    }
    pt_pprint(array_data(program_tree_t*, lst)[seq_sz - 1]);
  }
}

program_tree_t* pt_make_vector(arena_ptr_t pt_arena,
                               loc_t loc,
                               array_ptr_t /* [program_tree_t*] */ elems) {
  return pt_make_node(pt_arena, loc, PT_VECTOR,
                      (pt_value_t){.as_subtree_list = elems}, NULL);
}

bool is_atomic(program_tree_t* expr) {
  static const pt_kind_t atomics[] = {PT_BOOL_LITERAL, PT_INT_LITERAL, PT_NAME};
  return m_contains(expr->kind, atomics, sizeof(atomics) / sizeof(pt_kind_t));
}

bool is_primitive_literal(program_tree_t* expr) {
  static const pt_kind_t literals[] = {PT_BOOL_LITERAL, PT_INT_LITERAL};
  return m_contains(expr->kind, literals, sizeof(literals) / sizeof(pt_kind_t));
}

bool is_name(program_tree_t* expr) {
  static const pt_kind_t names[] = {PT_GLOBAL_SYMBOL, PT_NAME};
  return m_contains(expr->kind, names, sizeof(names) / sizeof(pt_kind_t));
}

static void pprint_pt_if(pt_if_form_t if_form) {
  printf("if ");
  pt_pprint(if_form.cond_subtree);
  printf(" then ");
  pt_pprint(if_form.t_branch_subtree);
  printf(" else ");
  pt_pprint(if_form.f_branch_subtree);
}

static void pprint_pt_call(pt_call_t call) {
  printf("(");
  pt_pprint(call.fn_subtree);
  printf(" ");
  pprint_pt_seq(call.args_subtrees, ' ');
  printf(")");
}

static const char* binop_repr(binop_t op) {
  static const char* reprs[] = {
      [BINOP_SUM] = "+",   [BINOP_SUB] = "-", [BINOP_MUL] = "*",
      [BINOP_DIV] = "/",   [BINOP_LE] = "<=", [BINOP_LT] = "<",
      [BINOP_GE] = ">=",   [BINOP_GT] = ">",  [BINOP_EQ] = "eq?",
      [BINOP_NEQ] = "neq?"};

  static_assert(sizeof(reprs) / sizeof(char*) == BINOP_COUNT__,
                "Some binops missing representations");

  return reprs[op];
}

static const char* uop_repr(uop_t op) {
  static const char* reprs[] = {[UNARY_NOT] = "not"};

  static_assert(sizeof(reprs) / sizeof(char*) == UNARY_COUNT__,
                "Some uops missing representations");

  return reprs[op];
}

static void pprint_pt_uop(pt_uop_t uop) {
  printf("(");
  printf("OP(%s) ", uop_repr(uop.op));
  pt_pprint(uop.operand);
  printf(")");
}

static void pprint_pt_binop(pt_binop_t binop) {
  printf("(OP(%s) ", binop_repr(binop.op));
  pt_pprint(binop.lhs);
  printf(" ");
  pt_pprint(binop.rhs);
  printf(")");
}

static void pprint_pt_let(pt_let_form_t let) {
  printf("let $var%d := ", let.bind.name_id);
  pt_pprint(let.bind.value_subtree);
  printf(" in ");
  pt_pprint(let.expr_subtree);
}

static void pprint_pt_lambda(pt_lambda_t lambda) {
  printf("lambda(id: %d)[", lambda.id);
  size_t captured_sz = array_size(lambda.captured);
  if (captured_sz > 0) {
    for (size_t i = 0; i < captured_sz - 1; i++)
      printf("$var%d ", array_data(int, lambda.captured)[i]);
    printf("$var%d", array_data(int, lambda.captured)[captured_sz - 1]);
  }
  printf("]");
  printf("\\");
  array_ptr_t params = lambda.params;
  size_t params_sz = array_size(params);

  for (size_t i = 0; i < params_sz; i++)
    printf("$var%d ", array_data(int, params)[i]);

  printf("-> ");
  pt_pprint(lambda.body_subtree);
}

static void pprint_pt_vector(array_ptr_t /* [program_tree_t*] */ elems) {
  printf("{");
  pprint_pt_seq(elems, ' ');
  printf("}");
}

void pt_pprint(program_tree_t* pt) {
  switch (pt->kind) {
    case PT_ERROR:;
      {
        printf("ERROR(%s)", pt->err_msg);
        break;
      }
    case PT_TOPLEVEL:;
      {
        pprint_pt_seq(pt->value.as_subtree_list, '\n');
        printf("\n");
        break;
      }
    case PT_BOOL_LITERAL:;
      {
        printf("BOOL(%s)", pt->value.as_bool ? "true" : "false");
        break;
      }
    case PT_INT_LITERAL:;
      {
        printf("INT(%lli)", pt->value.as_i64);
        break;
      }
    case PT_STR_LITERAL:;
      {
        printf("STRING(id: %d, \"%s\")", pt->value.as_str_literal.id,
               pt->value.as_str_literal.cstr);
        break;
      }
    case PT_NAME:;
      {
        printf("$var%d", pt->value.as_name_id);
        break;
      }
    case PT_UOP:;
      {
        pprint_pt_uop(pt->value.as_uop);
        break;
      }
    case PT_BINOP:
      pprint_pt_binop(pt->value.as_binop);
      break;
    case PT_IF:;
      {
        pprint_pt_if(pt->value.as_if_form);
        break;
      }
    case PT_CALL:;
      {
        pprint_pt_call(pt->value.as_call);
        break;
      }
    case PT_LET:;
      {
        pprint_pt_let(pt->value.as_let_form);
        break;
      }
    case PT_LAMBDA:;
      {
        pprint_pt_lambda(pt->value.as_lambda);
        break;
      }
    case PT_GLOBAL_SYMBOL:;
      {
        printf("GLOBAL(%s)", pt->value.as_symbol);
        break;
      }
    case PT_VECTOR:;
      {
        pprint_pt_vector(pt->value.as_subtree_list);
        break;
      }
  }
}

static void pt_seq_fmap(array_ptr_t pts, pt_fmap_op_t op) {
  for (size_t i = 0; i < array_size(pts); i++)
    array_data(program_tree_t*, pts)[i] =
        pt_fmap(array_data(program_tree_t*, pts)[i], op);
}

static void pt_seq_fmap_(array_ptr_t pts, pt_fmap_action_t action) {
  for (size_t i = 0; i < array_size(pts); i++)
    pt_fmap_(array_data(program_tree_t*, pts)[i], action);
}

program_tree_t* pt_fmap(program_tree_t* pt, pt_fmap_op_t op) {
  switch (pt->kind) {
    case PT_ERROR:
    case PT_GLOBAL_SYMBOL:
    case PT_NAME:
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
      break;
    case PT_VECTOR:
    case PT_TOPLEVEL:;
      {
        pt_seq_fmap(pt->value.as_subtree_list, op);
        break;
      }
    case PT_LET:;
      {
        pt->value.as_let_form.bind.value_subtree =
            pt_fmap(pt->value.as_let_form.bind.value_subtree, op);

        pt->value.as_let_form.expr_subtree =
            pt_fmap(pt->value.as_let_form.expr_subtree, op);
        break;
      }
    case PT_UOP:;
      {
        pt->value.as_uop.operand = pt_fmap(pt->value.as_uop.operand, op);
        break;
      }
    case PT_BINOP:;
      {
        pt->value.as_binop.lhs = pt_fmap(pt->value.as_binop.lhs, op);
        pt->value.as_binop.rhs = pt_fmap(pt->value.as_binop.rhs, op);
        break;
      }
    case PT_CALL:;
      {
        pt->value.as_call.fn_subtree =
            pt_fmap(pt->value.as_call.fn_subtree, op);
        pt_seq_fmap(pt->value.as_call.args_subtrees, op);
        break;
      }
    case PT_IF:;
      {
        pt->value.as_if_form.cond_subtree =
            pt_fmap(pt->value.as_if_form.cond_subtree, op);
        pt->value.as_if_form.t_branch_subtree =
            pt_fmap(pt->value.as_if_form.t_branch_subtree, op);
        pt->value.as_if_form.f_branch_subtree =
            pt_fmap(pt->value.as_if_form.f_branch_subtree, op);
        break;
      }
    case PT_LAMBDA:;
      {
        pt->value.as_lambda.body_subtree =
            pt_fmap(pt->value.as_lambda.body_subtree, op);
        break;
      }
  }

  return op(pt);
}

void pt_fmap_(program_tree_t* pt, pt_fmap_action_t action) {
  switch (pt->kind) {
    case PT_ERROR:
    case PT_GLOBAL_SYMBOL:
    case PT_NAME:
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
      break;
    case PT_VECTOR:
    case PT_TOPLEVEL:;
      {
        pt_seq_fmap_(pt->value.as_subtree_list, action);
        break;
      }
    case PT_LET:;
      {
        pt_fmap_(pt->value.as_let_form.bind.value_subtree, action);
        pt_fmap_(pt->value.as_let_form.expr_subtree, action);
        break;
      }
    case PT_UOP:;
      {
        pt_fmap_(pt->value.as_uop.operand, action);
        break;
      }
    case PT_BINOP:;
      {
        pt_fmap_(pt->value.as_binop.lhs, action);
        pt_fmap_(pt->value.as_binop.rhs, action);
        break;
      }
    case PT_CALL:;
      {
        pt_fmap_(pt->value.as_call.fn_subtree, action);
        pt_seq_fmap_(pt->value.as_call.args_subtrees, action);
        break;
      }
    case PT_IF:;
      {
        pt_fmap_(pt->value.as_if_form.cond_subtree, action);
        pt_fmap_(pt->value.as_if_form.t_branch_subtree, action);
        pt_fmap_(pt->value.as_if_form.f_branch_subtree, action);
        break;
      }
    case PT_LAMBDA:;
      {
        pt_fmap_(pt->value.as_lambda.body_subtree, action);
        break;
      }
  }
  action(pt);
}
