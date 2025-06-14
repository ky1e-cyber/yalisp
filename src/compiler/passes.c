#include <assert.h>
#include <stdnoreturn.h>
#include "allocator.h"
#include "arena.h"
#include "array.h"
#include "error.h"
#include "macros.h"
#include "passes.h"
#include "program_tree.h"
#include "shared.h"
#include "vector.h"

static noreturn void error_pass_buf() {
  error("Couldn't allocate buffer for transformation passes\n");
}

static bool is_atomic(program_tree_t* expr) {
  static const pt_kind_t atomics[] = {PT_BOOL_LITERAL, PT_INT_LITERAL,
                                      PT_STR_LITERAL, PT_NAME};
  return m_contains(expr->kind, atomics, sizeof(atomics) / sizeof(pt_kind_t));
}

static program_tree_t* to_mnf_expr(program_tree_t* pt, arena_ptr_t pt_arena);

typedef struct {
  vector_ptr_t /* [bind_pair_t] */ mappings;
  program_tree_t* atomic_pt;
} rco_pair_t;

static rco_pair_t rco_atom(program_tree_t* expr,
                           arena_ptr_t pt_arena,
                           vector_ptr_t acc) {
  program_tree_t* value;
  switch (expr->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
    case PT_NAME:
    case PT_GLOBAL_SYMBOL:
      return (rco_pair_t){.atomic_pt = expr, .mappings = acc};
    case PT_LAMBDA:;
      {
        program_tree_t* body = expr->value.as_lambda.body_subtree;
        expr->value.as_lambda.body_subtree = to_mnf_expr(body, pt_arena);
        value = expr;
        break;
      }
    case PT_LET:;
      {
        program_tree_t* let_mnf = to_mnf_expr(expr, pt_arena);
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
    case PT_CALL:
    case PT_BINOP:
    case PT_IF:
    case PT_VECTOR:;
      {
        value = to_mnf_expr(expr, pt_arena);
        break;
      }
    case PT_TOPLEVEL:
    case PT_ERROR:;
      {
        assert(false);
        m_unreachable;
      }
  }

  name_id_t id = g_names_cnt++;
  bind_pair_t bind = {.name_id = id, .value_subtree = value};
  acc = vector_push_back(bind_pair_t, acc, bind);

  return (rco_pair_t){.atomic_pt = pt_make_name(pt_arena, (loc_t){}, id),
                      .mappings = acc};
}

static program_tree_t* unfold_binds_tail(program_tree_t* expr,
                                         vector_ptr_t binds,
                                         arena_ptr_t pt_arena,
                                         size_t i) {
  program_tree_t* let = pt_make_let(pt_arena, (loc_t){},
                                    vector_data(bind_pair_t, binds)[i], expr);
  return i == 0 ? let : unfold_binds_tail(let, binds, pt_arena, i - 1);
}

static program_tree_t* unfold_binds(program_tree_t* expr,
                                    vector_ptr_t binds,
                                    arena_ptr_t pt_arena) {
  size_t binds_sz = vector_size(binds);
  return binds_sz == 0 ? expr
                       : unfold_binds_tail(expr, binds, pt_arena, binds_sz - 1);
}

static vector_ptr_t atomize_seq(array_ptr_t /* [program_tree_t*] */ exprs,
                                arena_ptr_t pt_arena,
                                vector_ptr_t binds) {
  for (size_t i = 0; i < array_size(exprs); i++) {
    program_tree_t* expr = array_data(program_tree_t*, exprs)[i];
    rco_pair_t rco_expr = rco_atom(expr, pt_arena, binds);
    binds = rco_expr.mappings;
    array_data(program_tree_t*, exprs)[i] = rco_expr.atomic_pt;
  }

  return binds;
}

static program_tree_t* to_mnf_expr(program_tree_t* expr, arena_ptr_t pt_arena) {
  vector_ptr_t binds m_cleanup(vector_cleanup) =
      vector_make(bind_pair_t, alloc_default, error_pass_buf);

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
            to_mnf_expr(expr->value.as_lambda.body_subtree, pt_arena);
        return expr;
      }
    case PT_LET:;
      {
        program_tree_t* let_expr = expr->value.as_let_form.expr_subtree;
        program_tree_t* let_bind_expr =
            expr->value.as_let_form.bind.value_subtree;
        expr->value.as_let_form.expr_subtree = to_mnf_expr(let_expr, pt_arena);
        expr->value.as_let_form.bind.value_subtree =
            to_mnf_expr(let_bind_expr, pt_arena);
        return expr;
      }
    case PT_CALL:;
      {
        rco_pair_t rco_fn =
            rco_atom(expr->value.as_call.fn_subtree, pt_arena, binds);
        binds = rco_fn.mappings;
        expr->value.as_call.fn_subtree = rco_fn.atomic_pt;

        binds = atomize_seq(expr->value.as_call.args_subtrees, pt_arena, binds);

        return unfold_binds(expr, binds, pt_arena);
      }
    case PT_VECTOR:;
      {
        binds = atomize_seq(expr->value.as_subtree_list, pt_arena, binds);
        return unfold_binds(expr, binds, pt_arena);
      }
    case PT_BINOP:;
      {
        program_tree_t* lhs = expr->value.as_binop.lhs;
        program_tree_t* rhs = expr->value.as_binop.rhs;

        rco_pair_t rco_lhs = rco_atom(lhs, pt_arena, binds);
        binds = rco_lhs.mappings;
        rco_pair_t rco_rhs = rco_atom(rhs, pt_arena, binds);
        binds = rco_rhs.mappings;

        expr->value.as_binop.lhs = rco_lhs.atomic_pt;
        expr->value.as_binop.rhs = rco_rhs.atomic_pt;

        return unfold_binds(expr, binds, pt_arena);
      }
    case PT_IF:;
      {
        rco_pair_t rco_cond =
            rco_atom(expr->value.as_if_form.cond_subtree, pt_arena, binds);
        binds = rco_cond.mappings;

        expr->value.as_if_form.cond_subtree = rco_cond.atomic_pt;

        expr->value.as_if_form.t_branch_subtree =
            to_mnf_expr(expr->value.as_if_form.t_branch_subtree, pt_arena);
        expr->value.as_if_form.f_branch_subtree =
            to_mnf_expr(expr->value.as_if_form.f_branch_subtree, pt_arena);

        return unfold_binds(expr, binds, pt_arena);
      }
    case PT_TOPLEVEL:
    case PT_ERROR:;
      {
        assert(false);
        m_unreachable;
      }
  }
}

program_tree_t* to_mnf(program_tree_t* pt_toplevel, arena_ptr_t pt_arena) {
  assert(pt_toplevel->kind == PT_TOPLEVEL);

  array_ptr_t exprs = pt_toplevel->value.as_subtree_list;
  size_t exprs_sz = array_size(exprs);

  for (size_t i = 0; i < exprs_sz; i++) {
    program_tree_t* e = array_data(program_tree_t*, exprs)[i];
    array_data(program_tree_t*, exprs)[i] = to_mnf_expr(e, pt_arena);
  }

  return pt_toplevel;
}
