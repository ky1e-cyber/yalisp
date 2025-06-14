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

const char* g_stdlib_builtin_globals[] = {"vector-ref", "vector-length"};
size_t g_stdlib_builtin_globals_sz = sizeof(g_globals_table) / sizeof(char*);

int g_names_cnt = 0;

arena_ptr_t g_str_arena = NULL;
arena_ptr_t g_pt_arena = NULL;
arena_ptr_t g_env_arena = NULL;

globals_table_t g_globals_table;

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
  arena_release(g_globals_table.arena);
  arena_release(g_pt_arena);
  arena_release(g_env_arena);
}

static vector_ptr_t register_lambdas_rec(program_tree_t* pt, vector_ptr_t acc);

static vector_ptr_t register_lambdas_seq(
    array_ptr_t /* [program_tree_t*] */ subtrees,
    vector_ptr_t acc) {
  for (size_t i = 0; i < array_size(subtrees); i++)
    acc = register_lambdas_rec(array_data(program_tree_t*, subtrees)[i], acc);

  return acc;
}

static vector_ptr_t register_lambdas_rec(program_tree_t* pt, vector_ptr_t acc) {
  switch (pt->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
    case PT_NAME:
    case PT_GLOBAL_SYMBOL:
    case PT_ERROR:
      return acc;
    case PT_LET:;
      {
        acc =
            register_lambdas_rec(pt->value.as_let_form.bind.value_subtree, acc);
        return register_lambdas_rec(pt->value.as_let_form.expr_subtree, acc);
      }
    case PT_BINOP:;
      {
        acc = register_lambdas_rec(pt->value.as_binop.lhs, acc);
        return register_lambdas_rec(pt->value.as_binop.rhs, acc);
      }
    case PT_CALL:;
      {
        acc = register_lambdas_rec(pt->value.as_call.fn_subtree, acc);
        return register_lambdas_seq(pt->value.as_call.args_subtrees, acc);
      }
    case PT_VECTOR:
      return register_lambdas_seq(pt->value.as_subtree_list, acc);
    case PT_IF:;
      {
        acc = register_lambdas_rec(pt->value.as_if_form.cond_subtree, acc);
        acc = register_lambdas_rec(pt->value.as_if_form.t_branch_subtree, acc);
        return register_lambdas_rec(pt->value.as_if_form.f_branch_subtree, acc);
      }
    case PT_LAMBDA:;
      {
        int id = g_lambdas_cnt++;
        pt->value.as_lambda.id = id;
        acc = vector_push_back(pt_lambda_t, acc, pt->value.as_lambda);
        return register_lambdas_rec(pt->value.as_lambda.body_subtree, acc);
      }
    case PT_TOPLEVEL:;
      {
        assert(false);
        m_unreachable;
      }
  }
}

void register_lambdas(program_tree_t* pt_toplevel) {
  assert(pt_toplevel->kind == PT_TOPLEVEL);

  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(pt_lambda_t, error_buf);
  buf = register_lambdas_seq(pt_toplevel->value.as_subtree_list, buf);

  g_lambdas = (pt_lambda_t*)malloc(vector_size(buf) * sizeof(pt_lambda_t));

  vector_copy_data(pt_lambda_t, buf, g_lambdas);
}
