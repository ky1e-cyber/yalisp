#include <assert.h>
#include <stddef.h>
#include <stdnoreturn.h>
#include "allocator.h"
#include "arena.h"
#include "defs.h"
#include "env_table.h"
#include "error.h"
#include "globals_table.h"
#include "shared.h"

const char* g_stdlib_builtin_globals[] = {"vector-ref", "vector-length"};

size_t g_stdlib_builtin_globals_sz = sizeof(g_globals_table) / sizeof(char*);

name_id_t g_names_cnt = 0;

arena_ptr_t g_str_arena = NULL;
arena_ptr_t g_pt_arena = NULL;
arena_ptr_t g_env_arena = NULL;

globals_table_t g_globals_table;

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

void shared_init(size_t str_arena_size,
                 size_t pt_arena_size,
                 size_t globals_arena_size,
                 size_t env_arena_size) {
  g_str_arena = arena_make(str_arena_size, alloc_default, error_str_arena);
  g_pt_arena = arena_make(pt_arena_size, alloc_default, error_pt_arena);

  g_globals_table = globals_table_make(
      arena_make(globals_arena_size, alloc_default, error_globals));

  for (size_t i = 0; i < g_stdlib_builtin_globals_sz; i++)
    g_globals_table =
        globals_table_add(g_globals_table, g_stdlib_builtin_globals[i]);

  g_env_arena = arena_make(env_arena_size, alloc_default, error_env_arena);
}

void shared_deinit() {
  arena_release(g_globals_table.arena);
  arena_release(g_pt_arena);
  arena_release(g_env_arena);
}
