#include <stdlib.h>
#include "allocator.h"

void* acquire_default(size_t size) {
  return malloc(size);
}

void* resize_default(void* mem, size_t new_size) {
  return realloc(mem, new_size);
}

void release_default(void* mem) {
  free(mem);
}

alloc_t alloc_default = {.acquire = acquire_default,
                         .resize = resize_default,
                         .release = release_default};
