#if !defined(H_SHARED)
#define H_SHARED

#include <stddef.h>
#include "globals_table.h"

typedef int name_id_t;
extern name_id_t g_names_cnt;

extern size_t g_parser_tree_arena_size;
extern size_t g_parser_env_arena_size;
extern size_t g_parser_globals_arena_size;

extern const char* g_stdlib_builtin_globals[];

extern size_t g_stdlib_builtin_globals_sz;

extern globals_table_t g_globals_table;

void fill_globals_table();

#endif
