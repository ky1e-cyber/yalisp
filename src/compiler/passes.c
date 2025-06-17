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

static void rename_global(char* symbol) {
  size_t l = strlen(symbol);

  for (size_t i = 0; i < l; i++) {
    if (symbol[i] == '-')
      symbol[i] = '_';
  }
}

static void rename_globals_action(program_tree_t* pt) {
  if (pt->kind == PT_GLOBAL_SYMBOL)
    rename_global(pt->value.as_symbol);
}

program_tree_t* rename_globals_pass(program_tree_t* pt_toplevel) {
  pt_fmap_(pt_toplevel, rename_globals_action);
  return pt_toplevel;
}

program_tree_t* register_pass(program_tree_t* pt_toplevel) {
  register_stuff(pt_toplevel);
  return pt_toplevel;
}

static program_tree_t* remove_logic_binops_op(program_tree_t* pt) {
  switch (pt->kind) {
    case PT_BINOP:;
      {
        switch (pt->value.as_binop.op) {
          case BINOP_AND:
            return pt_make_if(
                g_pt_arena, (loc_t){}, pt->value.as_binop.lhs,
                pt->value.as_binop.rhs,
                pt_make_bool_literal(g_pt_arena, (loc_t){}, false));

          case BINOP_OR:
            return pt_make_if(g_pt_arena, (loc_t){}, pt->value.as_binop.lhs,
                              pt_make_bool_literal(g_pt_arena, (loc_t){}, true),
                              pt->value.as_binop.rhs);
          default:
            break;
        }
        break;
      }
    default:
      break;
  }

  return pt;
}

program_tree_t* remove_logic_operators_pass(program_tree_t* pt_toplevel) {
  return pt_fmap(pt_toplevel, remove_logic_binops_op);
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

static program_tree_t* to_mnf_op(program_tree_t* pt) {
  vector_ptr_t binds m_cleanup(vector_cleanup) =
      vector_make(bind_pair_t, error_pass_buf);

  switch (pt->kind) {
    case PT_CALL:;
      {
        rco_pair_t rco_fn = rco_atom(pt->value.as_call.fn_subtree, binds);
        binds = rco_fn.mappings;
        pt->value.as_call.fn_subtree = rco_fn.atomic_pt;
        binds = atomize_seq(pt->value.as_call.args_subtrees, binds);
        return unfold_binds(pt, binds);
      }
    case PT_VECTOR:;
      {
        binds = atomize_seq(pt->value.as_subtree_list, binds);
        return unfold_binds(pt, binds);
      }
    case PT_UOP:;
      {
        program_tree_t* operand = pt->value.as_uop.operand;
        rco_pair_t rco_operand = rco_atom(operand, binds);
        binds = rco_operand.mappings;
        pt->value.as_uop.operand = rco_operand.atomic_pt;
        return unfold_binds(pt, binds);
      }
    case PT_BINOP:;
      {
        program_tree_t* lhs = pt->value.as_binop.lhs;
        program_tree_t* rhs = pt->value.as_binop.rhs;

        rco_pair_t rco_lhs = rco_atom(lhs, binds);
        binds = rco_lhs.mappings;
        rco_pair_t rco_rhs = rco_atom(rhs, binds);
        binds = rco_rhs.mappings;

        pt->value.as_binop.lhs = rco_lhs.atomic_pt;
        pt->value.as_binop.rhs = rco_rhs.atomic_pt;

        return unfold_binds(pt, binds);
      }
    case PT_IF:;
      {
        rco_pair_t rco_cond =
            rco_atom(pt->value.as_if_form.cond_subtree, binds);
        binds = rco_cond.mappings;
        pt->value.as_if_form.cond_subtree = rco_cond.atomic_pt;
        return unfold_binds(pt, binds);
      }
    default:
      break;
  }

  return pt;
}

static program_tree_t* to_mnf_expr(program_tree_t* expr) {
  return pt_fmap(expr, to_mnf_op);
}

program_tree_t* to_mnf_pass(program_tree_t* pt_toplevel) {
  return to_mnf_expr(pt_toplevel);
}
