#include <assert.h>
#include <stddef.h>
#include "defs.h"
#include "shared.h"

size_t g_parser_tree_arena_size = PARSER_TREE_ARENA_SIZE;
size_t g_parser_env_arena_size = PARSER_ENV_ARENA_SIZE;

name_id_t g_names_cnt = 0;
