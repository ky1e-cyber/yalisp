#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include "arena.h"
#include "array.h"
#include "error.h"
#include "globals_table.h"
#include "macros.h"
#include "parser.h"
#include "shared.h"
#include "vector.h"

const char* g_stdlib_builtin_globals[] = {"print", "println", "read-int",
                                          "vector-ref", "vector-length"};
const size_t g_stdlib_builtin_globals_sz =
    sizeof(g_stdlib_builtin_globals) / sizeof(char*);

int g_names_cnt = 0;

arena_ptr_t g_str_arena = NULL;
arena_ptr_t g_pt_arena = NULL;
arena_ptr_t g_env_arena = NULL;

globals_table_t g_globals_table;

int g_str_cnt = 0;
pt_str_literal_t* g_str_literals = NULL;

int g_lambdas_cnt = 0;
pt_lambda_t* g_lambdas = NULL;

static noreturn void error_str_arena() {
  error("String arena allocation failure\n");
}

static noreturn void error_pt_arena() {
  error("Program tree arena allocation failure\n");
}

static noreturn void error_globals() {
  error("Globals env table allocation failure\n");
}

static noreturn void error_env_arena() {
  error("Local env table allocation failure\n");
}

static noreturn void error_buf() {
  error("Buffer allocation failure\n");
}

void shared_init(size_t str_arena_size,
                 size_t pt_arena_size,
                 size_t globals_arena_size,
                 size_t env_arena_size) {
  g_str_arena = arena_make(str_arena_size, error_str_arena);
  g_pt_arena = arena_make(pt_arena_size, error_pt_arena);

  g_globals_table =
      globals_table_make(arena_make(globals_arena_size, error_globals));

  for (size_t i = 0; i < g_stdlib_builtin_globals_sz; i++)
    g_globals_table =
        globals_table_add(g_globals_table, g_stdlib_builtin_globals[i]);

  g_env_arena = arena_make(env_arena_size, error_env_arena);
}

void shared_deinit() {
  free(g_lambdas);
  free(g_str_literals);
  arena_release(g_globals_table.arena);
  arena_release(g_pt_arena);
  arena_release(g_env_arena);
}

static vector_ptr_t g_register_lambdas_acc;
static vector_ptr_t g_register_strings_acc;

static void register_rec(program_tree_t* pt);

static void register_seq(array_ptr_t /* [program_tree_t*] */ subtrees) {
  for (size_t i = 0; i < array_size(subtrees); i++)
    register_rec(array_data(program_tree_t*, subtrees)[i]);
}

static void register_rec(program_tree_t* pt) {
  switch (pt->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_NAME:
    case PT_GLOBAL_SYMBOL:
    case PT_ERROR:
      return;
    case PT_LET:;
      {
        register_rec(pt->value.as_let_form.bind.value_subtree);
        register_rec(pt->value.as_let_form.expr_subtree);
        return;
      }
    case PT_UOP:;
      {
        register_rec(pt->value.as_uop.operand);
        return;
      }
    case PT_BINOP:;
      {
        register_rec(pt->value.as_binop.lhs);
        register_rec(pt->value.as_binop.rhs);
        return;
      }
    case PT_CALL:;
      {
        register_rec(pt->value.as_call.fn_subtree);
        register_seq(pt->value.as_call.args_subtrees);
        return;
      }
    case PT_VECTOR:;
      {
        register_seq(pt->value.as_subtree_list);
        return;
      }
    case PT_IF:;
      {
        register_rec(pt->value.as_if_form.cond_subtree);
        register_rec(pt->value.as_if_form.t_branch_subtree);
        register_rec(pt->value.as_if_form.f_branch_subtree);
        return;
      }
    case PT_STR_LITERAL:;
      {
        pt->value.as_str_literal.id = next_str_id();
        g_register_strings_acc = vector_push_back(
            pt_str_literal_t, g_register_strings_acc, pt->value.as_str_literal);
        return;
      }
    case PT_LAMBDA:;
      {
        pt->value.as_lambda.id = next_lambda_id();
        g_register_lambdas_acc = vector_push_back(
            pt_lambda_t, g_register_lambdas_acc, pt->value.as_lambda);
        register_rec(pt->value.as_lambda.body_subtree);
        return;
      }
    case PT_TOPLEVEL:;
      {
        assert(false);
        m_unreachable;
      }
  }
}

int next_name_id() {
  return (g_names_cnt++) + 1;
}

int next_str_id() {
  return (g_str_cnt++) + 1;
}

int next_lambda_id() {
  return (g_lambdas_cnt++) + 1;
}

void register_stuff(program_tree_t* pt_toplevel) {
  assert(pt_toplevel->kind == PT_TOPLEVEL);

  g_register_lambdas_acc = vector_make(pt_lambda_t, error_buf);
  g_register_strings_acc = vector_make(pt_str_literal_t, error_buf);
  register_seq(pt_toplevel->value.as_subtree_list);

  g_lambdas = (pt_lambda_t*)malloc(vector_size(g_register_lambdas_acc) *
                                   sizeof(pt_lambda_t));
  g_str_literals = (pt_str_literal_t*)malloc(
      vector_size(g_register_strings_acc) * sizeof(pt_str_literal_t));

  vector_copy_data(pt_lambda_t, g_register_lambdas_acc, g_lambdas);
  vector_copy_data(pt_str_literal_t, g_register_strings_acc, g_str_literals);

  vector_release(g_register_lambdas_acc);
  vector_release(g_register_strings_acc);
}
