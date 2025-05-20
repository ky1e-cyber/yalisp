#if !defined(H_ALLOCATOR)
#define H_ALLOCATOR

#include <stddef.h>

typedef struct alloc_t_ {
  void* (*const acquire)(size_t);
  void* (*const resize)(void*, size_t);
  void (*const release)(void*);
} alloc_t;

extern alloc_t alloc_default;

#endif
