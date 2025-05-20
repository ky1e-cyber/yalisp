#include <assert.h>
#include "allocator.h"
#include "arena.h"
#include "error.h"
#include "macros.h"
#include "parser.h"

void arena_error() {
  error("Arena allocation error\n");
}

int main(int argc, char* argv[]) {
  assert(argc >= 2);

  const char* const src_path = argv[1];

  arena_ptr_t str_arena m_cleanup(arena_cleanup) =
      arena_make(1 << 12, alloc_default, arena_error);

  arena_ptr_t pt_arena m_cleanup(arena_cleanup) =
      arena_make(1 << 12, alloc_default, arena_error);

  fdump_parse_tree(stdout, src_path, str_arena, pt_arena);

  return 0;
}
