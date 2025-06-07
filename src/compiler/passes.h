#if !defined(H_PASSES)
#define H_PASSES

#include "arena.h"
#include "program_tree.h"

program_tree_t* to_mnf(program_tree_t* ast, arena_ptr_t arena);

#endif
