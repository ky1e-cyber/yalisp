#include <assert.h>
#include <stdnoreturn.h>
#include "array.h"
#include "error.h"
#include "macros.h"
#include "passes.h"
#include "program_tree.h"
#include "shared.h"
#include "vector.h"

static noreturn void error_pass_buf() {
  error("Allocation failure in buffer for transformation passes\n");
}

program_tree_t* register_pass(program_tree_t* pt_toplevel) {
  register_stuff(pt_toplevel);
  return pt_toplevel;
}

static program_tree_t* shrink_rec(program_tree_t* pt);

static void shrink_seq(array_ptr_t /* [program_tree_t*] */ subtrees) {
  for (size_t i = 0; i < array_size(subtrees); i++)
    array_data(program_tree_t*, subtrees)[i] =
        shrink_rec(array_data(program_tree_t*, subtrees)[i]);
}

static program_tree_t* shrink_rec(program_tree_t* pt) {
  switch (pt->kind) {
    case PT_ERROR:
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
    case PT_NAME:
    case PT_GLOBAL_SYMBOL:
      return pt;
    case PT_UOP:;
      {
        pt->value.as_uop.operand = shrink_rec(pt->value.as_uop.operand);
        return pt;
      }
    case PT_LET:;
      {
        program_tree_t* bind_val = pt->value.as_let_form.bind.value_subtree;
        program_tree_t* val = pt->value.as_let_form.expr_subtree;

        pt->value.as_let_form.bind.value_subtree = shrink_rec(bind_val);
        pt->value.as_let_form.expr_subtree = shrink_rec(val);

        return pt;
      }
    case PT_LAMBDA:;
      {
        pt->value.as_lambda.body_subtree =
            shrink_rec(pt->value.as_lambda.body_subtree);

        return pt;
      }
    case PT_CALL:;
      {
        pt->value.as_call.fn_subtree = shrink_rec(pt->value.as_call.fn_subtree);
        shrink_seq(pt->value.as_call.args_subtrees);
        return pt;
      }
    case PT_IF:;
      {
        pt->value.as_if_form.cond_subtree =
            shrink_rec(pt->value.as_if_form.cond_subtree);
        pt->value.as_if_form.t_branch_subtree =
            shrink_rec(pt->value.as_if_form.t_branch_subtree);
        pt->value.as_if_form.f_branch_subtree =
            shrink_rec(pt->value.as_if_form.f_branch_subtree);
        return pt;
      }
    case PT_VECTOR:
    case PT_TOPLEVEL:;
      {
        shrink_seq(pt->value.as_subtree_list);
        return pt;
      }
    case PT_BINOP: {
      pt->value.as_binop.lhs = shrink_rec(pt->value.as_binop.lhs);
      pt->value.as_binop.rhs = shrink_rec(pt->value.as_binop.rhs);

      binop_t op = pt->value.as_binop.op;
      program_tree_t* lhs = pt->value.as_binop.lhs;
      program_tree_t* rhs = pt->value.as_binop.rhs;

      switch (op) {
        case BINOP_AND:
          return pt_make_if(g_pt_arena, (loc_t){}, lhs, rhs,
                            pt_make_bool_literal(g_pt_arena, (loc_t){}, false));

        case BINOP_OR:
          return pt_make_if(g_pt_arena, (loc_t){}, lhs,
                            pt_make_bool_literal(g_pt_arena, (loc_t){}, true),
                            rhs);

        default:
          break;
      }

      return pt;
    }
  }
}

program_tree_t* shrink_logic_operators_pass(program_tree_t* pt_toplevel) {
  assert(pt_toplevel->kind == PT_TOPLEVEL);

  return shrink_rec(pt_toplevel);
}

static bool is_atomic(program_tree_t* expr) {
  static const pt_kind_t atomics[] = {PT_BOOL_LITERAL, PT_INT_LITERAL, PT_NAME};
  return m_contains(expr->kind, atomics, sizeof(atomics) / sizeof(pt_kind_t));
}

static program_tree_t* to_mnf_expr(program_tree_t* pt);

typedef struct {
  vector_ptr_t /* [bind_pair_t] */ mappings;
  program_tree_t* atomic_pt;
} rco_pair_t;

static rco_pair_t rco_atom(program_tree_t* expr, vector_ptr_t acc) {
  program_tree_t* value;
  switch (expr->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_NAME:
    case PT_GLOBAL_SYMBOL:
      return (rco_pair_t){.atomic_pt = expr, .mappings = acc};
    case PT_LAMBDA:;
      {
        program_tree_t* body = expr->value.as_lambda.body_subtree;
        expr->value.as_lambda.body_subtree = to_mnf_expr(body);
        value = expr;
        break;
      }
    case PT_LET:;
      {
        program_tree_t* let_mnf = to_mnf_expr(expr);
        if (let_mnf->kind == PT_LET) {
          program_tree_t* let_expr = let_mnf->value.as_let_form.expr_subtree;
          if (is_atomic(let_expr)) {
            acc = vector_push_back(bind_pair_t, acc,
                                   let_mnf->value.as_let_form.bind);
            return (rco_pair_t){.atomic_pt = let_expr, .mappings = acc};
          }
        }
        value = let_mnf;
        break;
      }

    case PT_STR_LITERAL:
    case PT_CALL:
    case PT_UOP:
    case PT_BINOP:
    case PT_IF:
    case PT_VECTOR:;
      {
        value = to_mnf_expr(expr);
        break;
      }
    case PT_TOPLEVEL:
    case PT_ERROR:;
      {
        assert(false);
        m_unreachable;
      }
  }

  int id = next_name_id();
  bind_pair_t bind = {.name_id = id, .value_subtree = value};
  acc = vector_push_back(bind_pair_t, acc, bind);

  return (rco_pair_t){.atomic_pt = pt_make_name(g_pt_arena, (loc_t){}, id),
                      .mappings = acc};
}

static program_tree_t* unfold_binds_tail(program_tree_t* expr,
                                         vector_ptr_t binds,
                                         size_t i) {
  program_tree_t* let = pt_make_let(g_pt_arena, (loc_t){},
                                    vector_data(bind_pair_t, binds)[i], expr);
  return i == 0 ? let : unfold_binds_tail(let, binds, i - 1);
}

static program_tree_t* unfold_binds(program_tree_t* expr, vector_ptr_t binds) {
  size_t binds_sz = vector_size(binds);
  return binds_sz == 0 ? expr : unfold_binds_tail(expr, binds, binds_sz - 1);
}

static vector_ptr_t atomize_seq(array_ptr_t /* [program_tree_t*] */ exprs,
                                vector_ptr_t binds) {
  for (size_t i = 0; i < array_size(exprs); i++) {
    program_tree_t* expr = array_data(program_tree_t*, exprs)[i];
    rco_pair_t rco_expr = rco_atom(expr, binds);
    binds = rco_expr.mappings;
    array_data(program_tree_t*, exprs)[i] = rco_expr.atomic_pt;
  }

  return binds;
}

static program_tree_t* to_mnf_expr(program_tree_t* expr) {
  vector_ptr_t binds m_cleanup(vector_cleanup) =
      vector_make(bind_pair_t, error_pass_buf);

  switch (expr->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
    case PT_NAME:
    case PT_GLOBAL_SYMBOL:
      return expr;
    case PT_LAMBDA:;
      {
        expr->value.as_lambda.body_subtree =
            to_mnf_expr(expr->value.as_lambda.body_subtree);
        return expr;
      }
    case PT_LET:;
      {
        program_tree_t* let_expr = expr->value.as_let_form.expr_subtree;
        program_tree_t* let_bind_expr =
            expr->value.as_let_form.bind.value_subtree;
        expr->value.as_let_form.expr_subtree = to_mnf_expr(let_expr);
        expr->value.as_let_form.bind.value_subtree = to_mnf_expr(let_bind_expr);
        return expr;
      }
    case PT_CALL:;
      {
        rco_pair_t rco_fn = rco_atom(expr->value.as_call.fn_subtree, binds);
        binds = rco_fn.mappings;
        expr->value.as_call.fn_subtree = rco_fn.atomic_pt;

        binds = atomize_seq(expr->value.as_call.args_subtrees, binds);

        return unfold_binds(expr, binds);
      }
    case PT_VECTOR:;
      {
        binds = atomize_seq(expr->value.as_subtree_list, binds);
        return unfold_binds(expr, binds);
      }
    case PT_UOP:;
      {
        program_tree_t* operand = expr->value.as_uop.operand;
        rco_pair_t rco_operand = rco_atom(operand, binds);
        binds = rco_operand.mappings;

        expr->value.as_uop.operand = rco_operand.atomic_pt;

        return unfold_binds(expr, binds);
      }
    case PT_BINOP:;
      {
        program_tree_t* lhs = expr->value.as_binop.lhs;
        program_tree_t* rhs = expr->value.as_binop.rhs;

        rco_pair_t rco_lhs = rco_atom(lhs, binds);
        binds = rco_lhs.mappings;
        rco_pair_t rco_rhs = rco_atom(rhs, binds);
        binds = rco_rhs.mappings;

        expr->value.as_binop.lhs = rco_lhs.atomic_pt;
        expr->value.as_binop.rhs = rco_rhs.atomic_pt;

        return unfold_binds(expr, binds);
      }
    case PT_IF:;
      {
        rco_pair_t rco_cond =
            rco_atom(expr->value.as_if_form.cond_subtree, binds);
        binds = rco_cond.mappings;

        expr->value.as_if_form.cond_subtree = rco_cond.atomic_pt;

        expr->value.as_if_form.t_branch_subtree =
            to_mnf_expr(expr->value.as_if_form.t_branch_subtree);
        expr->value.as_if_form.f_branch_subtree =
            to_mnf_expr(expr->value.as_if_form.f_branch_subtree);

        return unfold_binds(expr, binds);
      }
    case PT_TOPLEVEL:
    case PT_ERROR:;
      {
        assert(false);
        m_unreachable;
      }
  }
}

program_tree_t* to_mnf_pass(program_tree_t* pt_toplevel) {
  assert(pt_toplevel->kind == PT_TOPLEVEL);

  array_ptr_t exprs = pt_toplevel->value.as_subtree_list;
  size_t exprs_sz = array_size(exprs);

  for (size_t i = 0; i < exprs_sz; i++) {
    program_tree_t* e = array_data(program_tree_t*, exprs)[i];
    array_data(program_tree_t*, exprs)[i] = to_mnf_expr(e);
  }

  return pt_toplevel;
}
