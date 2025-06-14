#if !defined(H_SHARED)
#define H_SHARED

#include <stddef.h>

typedef int name_id_t;
extern name_id_t g_names_cnt;

extern size_t g_parser_tree_arena_size;
extern size_t g_parser_env_arena_size;

#endif
