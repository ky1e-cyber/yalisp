#if !defined(H_SHARED)
#define H_SHARED

#include <stddef.h>
#include "arena.h"
#include "globals_table.h"
#include "program_tree.h"

extern int g_names_cnt;

extern size_t g_pt_arena_size;
extern size_t g_parser_env_arena_size;
extern size_t g_globals_arena_size;
extern size_t g_str_arena_size;

extern const char* g_stdlib_builtin_globals[];
extern const size_t g_stdlib_builtin_globals_sz;

extern arena_ptr_t g_str_arena;
extern arena_ptr_t g_pt_arena;
extern arena_ptr_t g_env_arena;

extern globals_table_t g_globals_table;

extern int g_lambdas_cnt;
extern pt_lambda_t* g_lambdas;

void shared_init(size_t str_arena_size,
                 size_t pt_arena_size,
                 size_t globals_arena_size,
                 size_t env_arena_size);

void shared_deinit();

void register_lambdas(program_tree_t* pt_toplevel);

#endif
