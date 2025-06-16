#if !defined(H_PASSES)
#define H_PASSES

#include "program_tree.h"

program_tree_t* register_pass(program_tree_t* pt_toplevel);

program_tree_t* remove_logic_operators_pass(program_tree_t* pt_toplevel);

program_tree_t* to_mnf_pass(program_tree_t* pt_toplevel);

#endif
