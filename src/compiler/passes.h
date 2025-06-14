#if !defined(H_PASSES)
#define H_PASSES

#include "arena.h"
#include "program_tree.h"

program_tree_t* to_mnf(program_tree_t* pt_toplevel, arena_ptr_t pt_arena);

#endif
