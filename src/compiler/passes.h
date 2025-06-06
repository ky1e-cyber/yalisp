#if !defined(H_PASSES)
#define H_PASSES

#include "arena.h"
#include "ast.h"

ast_t* to_mnf(ast_t* ast, arena_ptr_t arena);

#endif
