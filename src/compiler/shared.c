#include <assert.h>
#include <stddef.h>
#include <stdnoreturn.h>
#include "defs.h"
#include "error.h"
#include "globals_table.h"
#include "shared.h"

const char* g_stdlib_builtin_globals[] = {"vector-ref", "vector-length"};

size_t g_stdlib_builtin_globals_sz = sizeof(g_globals_table) / sizeof(char*);

size_t g_parser_tree_arena_size = PARSER_TREE_ARENA_SIZE;
size_t g_parser_env_arena_size = PARSER_ENV_ARENA_SIZE;
size_t g_parser_globals_arena_size = PARSER_GLOBALS_ARENA_SIZE;

name_id_t g_names_cnt = 0;

globals_table_t g_globals_table;

static noreturn void error_globals() {
  error("Couldn't allocate globals env buffer\n");
}

void fill_globals_table() {
  g_globals_table = globals_table_make(
      arena_make(g_parser_globals_arena_size, alloc_default, error_globals));

  for (size_t i = 0; i < g_stdlib_builtin_globals_sz; i++)
    g_globals_table =
        globals_table_add(g_globals_table, g_stdlib_builtin_globals[i]);
}
