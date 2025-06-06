#include <stddef.h>
#include <string.h>
#include "arena.h"
#include "defs.h"
#include "memory.h"

arena_ptr_t arena_make(size_t sz, alloc_t alloc, void (*fail_callback)(void)) {
  arena_ptr_t arena = (arena_ptr_t)alloc.acquire(sizeof(arena_header_t) + sz);

  if (arena == NULL) {
    fail_callback();
    return NULL;
  }

  arena_header_t header = {
      .allocator = alloc, .fail_callback = fail_callback, .size = sz, .pos = 0};
  memcpy(arena, &header, sizeof(arena_header_t));

  return arena;
}

void arena_release(arena_ptr_t arena) {
  arena_get_allocator(arena)->release(arena);
}

void* arena_alloc(arena_ptr_t arena, size_t sz) {
  if (!arena_fits(arena, sz)) {
    arena->fail_callback();
    return NULL;
  }

  void* mem = arena_baseptr(arena) + arena->pos;

  size_t padded = mem_align_by(sz, MAX_ALIGNMENT);

  arena->pos +=
      arena->pos + padded > arena->size ? arena->size - arena->pos : padded;

  return mem;
}
