#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include "arena.h"
#include "error.h"
#include "globals_table.h"
#include "parser.h"
#include "shared.h"
#include "vector.h"

const char* g_stdlib_builtin_globals[] = {"print", "print-string", "read-int",
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

int next_name_id() {
  return (g_names_cnt++) + 1;
}

int next_str_id() {
  return (g_str_cnt++) + 1;
}

int next_lambda_id() {
  return (g_lambdas_cnt++) + 1;
}

static void register_action(program_tree_t* pt) {
  switch (pt->kind) {
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
        return;
      }
    default:
      return;
  }
}

void register_stuff(program_tree_t* pt_toplevel) {
  g_register_lambdas_acc = vector_make(pt_lambda_t, error_buf);
  g_register_strings_acc = vector_make(pt_str_literal_t, error_buf);

  pt_fmap_(pt_toplevel, register_action);

  g_lambdas = (pt_lambda_t*)malloc(vector_size(g_register_lambdas_acc) *
                                   sizeof(pt_lambda_t));
  g_str_literals = (pt_str_literal_t*)malloc(
      vector_size(g_register_strings_acc) * sizeof(pt_str_literal_t));

  vector_copy_data(pt_lambda_t, g_register_lambdas_acc, g_lambdas);
  vector_copy_data(pt_str_literal_t, g_register_strings_acc, g_str_literals);

  vector_release(g_register_lambdas_acc);
  vector_release(g_register_strings_acc);
}
