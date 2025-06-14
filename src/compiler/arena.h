#if !defined(H_ARENA)
#define H_ARENA

#include <stdbool.h>
#include "defs.h"
#include "macros.h"

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  void (*fail_callback)(void);
  size_t size;
  size_t pos;
} arena_header_t;

typedef arena_header_t* arena_ptr_t;

arena_ptr_t arena_make(size_t sz, void (*fail_callback)(void));

void arena_release(arena_ptr_t arena);

m_macro_like void arena_cleanup(arena_ptr_t* arenaptr) {
  arena_release(*arenaptr);
}

m_macro_like void* arena_baseptr(arena_ptr_t arena) {
  return (void*)(arena + 1);
}

m_macro_like size_t arena_size(arena_ptr_t arena) {
  return arena->size;
}

m_macro_like bool arena_fits(arena_ptr_t arena, size_t sz) {
  return (arena->size - arena->pos) >= sz;
}

void* arena_alloc(arena_ptr_t arena, size_t sz);

#endif
