#include <assert.h>
#include <stdio.h>
#include "allocator.h"
#include "arena.h"
#include "error.h"
#include "macros.h"
#include "parser.h"

void error_arena_alloc() {
  error("Arena allocation error\n");
}

int main(int argc, char* argv[]) {
  assert(argc >= 2);

  const char* const src_path = argv[1];

  arena_ptr_t str_arena m_cleanup(arena_cleanup) =
      arena_make(1 << 20, alloc_default, error_arena_alloc);

  arena_ptr_t ast_arena m_cleanup(arena_cleanup) =
      arena_make(1 << 20, alloc_default, error_arena_alloc);

  fdump_ast(stdout, src_path, ast_arena, str_arena);

  // parse_tree_t* pt = parse(src_path, str_arena, pt_arena);

  // dump_parser_errors_and_die_check(pt);

  return 0;
}
