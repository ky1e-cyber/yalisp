#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "defs.h"
#include "vector.h"

vector_ptr_t vector_make_sized(size_t elem_sz, void (*fail_callback)(void)) {
  vector_ptr_t vec = (vector_ptr_t)malloc(
      vector_bytesize_sized(elem_sz, VECTOR_INIT_CAPACITY));
  if (vec == NULL) {
    fail_callback();
    return NULL;
  }

  vector_header_t vec_hd = {.capacity = VECTOR_INIT_CAPACITY, .size = 0};
  memcpy(vec, &vec_hd, sizeof(vector_header_t));

  return vec;
}

void vector_release(vector_ptr_t vec) {
  free(vec);
}

vector_ptr_t vector_grow_sized(vector_ptr_t vec, size_t elem_sz) {
  const size_t old_cap = vector_capacity(vec);
  const size_t new_cap = old_cap * VECTOR_GROW_FACTOR;
  vector_ptr_t new_mem =
      new_cap <= old_cap  // relies on -fwrapv
          ? NULL
          : (vector_ptr_t)(realloc(vec,
                                   vector_bytesize_sized(elem_sz, new_cap)));
  if (new_mem == NULL) {
    vec->fail_callback();
  } else {
    new_mem->capacity = new_cap;
  }

  return new_mem;
}

vector_ptr_t vector_copy_sized(size_t elem_sz, vector_ptr_t src) {
  size_t cap = src->capacity;
  size_t sz = src->size;

  vector_ptr_t new = (vector_ptr_t)malloc(elem_sz * cap);
  if (new == NULL) {
    src->fail_callback();
    return NULL;
  }

  vector_header_t new_hd = {
      .capacity = cap, .size = sz, .fail_callback = src->fail_callback};
  memcpy(new, &new_hd, sizeof(vector_header_t));

  size_t bytesz = sz * elem_sz;

  for (size_t i = 0; i < bytesz; i++) {
    ((char*)vector_baseptr(new))[i] = ((char*)vector_baseptr(src))[i];
  }

  return new;
}

void vector_copy_data_sized(size_t elem_sz, vector_ptr_t vec, void* dest) {
  size_t bytesize = elem_sz * vec->size;
  memcpy(dest, vector_baseptr(vec), bytesize);
}
