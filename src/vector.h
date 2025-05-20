#if !defined(H_VECTOR)
#define H_VECTOR

#include <stdbool.h>
#include <stddef.h>
#include "allocator.h"
#include "defs.h"
#include "macros.h"

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  const alloc_t allocator;
  void (*fail_callback)(void);
  size_t capacity;
  size_t size;
} vector_header_t;

typedef vector_header_t* vector_ptr_t;

m_macro_like size_t vector_bytesize_sized(size_t elem_sz, size_t cnt) {
  return sizeof(vector_header_t) + elem_sz * cnt;
}

#define vector_bytesize(T, cnt) /* -> size_t */ \
  ({                                            \
    m_assert_istype(T);                         \
    size_t cnt__ = cnt;                         \
    vector_bytesize_sized(sizeof(T), cnt__);    \
  })

vector_ptr_t vector_make_sized(size_t elem_sz,
                               alloc_t alloc,
                               void (*fail_callback)(void));

#define vector_make(T, alloc, fail_callback) /* -> vector_ptr_t */ \
  ({                                                               \
    m_assert_istype(T);                                            \
    alloc_t alloc__ = alloc;                                       \
    vector_make_sized(sizeof(T), alloc__, fail_callback);          \
  })

void vector_release(vector_ptr_t vec);

m_macro_like void vector_cleanup(vector_ptr_t* vecptr) {
  vector_release(*vecptr);
}

m_macro_like size_t vector_size(vector_ptr_t vec) {
  return vec->size;
}

m_macro_like size_t vector_capacity(vector_ptr_t vec) {
  return vec->capacity;
}

m_macro_like void* vector_baseptr(vector_ptr_t vec) {
  return (void*)(vec + 1);
}

m_macro_like alloc_t vector_get_allocator(vector_ptr_t vec) {
  return vec->allocator;
}

#define vector_data(T, vec) /* -> T* */ \
  ({                                    \
    m_assert_istype(T);                 \
    vector_ptr_t vec__ = vec;           \
    (T*)vector_baseptr(vec__);          \
  })

vector_ptr_t vector_grow_sized(vector_ptr_t vec, size_t elem_sz);

#define vector_grow(T, vec) /* -> vector_ptr_t */ \
  ({                                              \
    m_assert_istype(T);                           \
    vector_grow_sized(vec, sizeof(T));            \
  })

#define vector_push_back(T, vec, elem) /* -> vector_ptr_t */              \
  ({                                                                      \
    m_assert_istype(T);                                                   \
    T elem__ = elem;                                                      \
    vector_ptr_t vec__ = vec;                                             \
    vector_ptr_t grown__ = (vector_size(vec__) == vector_capacity(vec__)) \
                               ? vector_grow_sized(vec__, sizeof(T))      \
                               : vec__;                                   \
    if (grown__ != NULL) {                                                \
      ((T*)vector_baseptr(grown__))[(grown__->size)++] = (T)elem__;       \
    }                                                                     \
    grown__;                                                              \
  })

m_macro_like void vector_remove_back(vector_ptr_t vec) {
  vec->size--;
}

#define vector_pop_back(T, vec) /* -> T */                         \
  ({                                                               \
    m_assert_istype(T);                                            \
    vector_ptr_t vec__ = vec;                                      \
    T res__ = ((T*)vector_baseptr(vec__))[vector_size(vec__) - 1]; \
    vector_remove_back(vec__);                                     \
    res__;                                                         \
  })

#define vector_begin(T, vec) /* -> T* */ \
  ({                                     \
    m_assert_istype(T);                  \
    vector_ptr_t vec__ = vec;            \
    (T*)vector_baseptr(vec__);           \
  })

#define vector_end(T, vec) /* -> T* */                \
  ({                                                  \
    m_assert_istype(T);                               \
    vector_ptr_t vec__ = vec;                         \
    ((T*)vector_baseptr(vec__)) + vector_size(vec__); \
  })

#define vector_foreach(T, vec, op) /* -> void */ \
  {                                              \
    m_assert_istype(T);                          \
    vector_ptr_t vec__ = vec;                    \
    void (*op__)(T*) = op;                       \
    T* begin__ = (T*)vector_baseptr(vec__);      \
    T* end__ = begin__ + vector_size(vec__);     \
    for (T* it = begin__; it < end__; it++) {    \
      op__(it);                                  \
    }                                            \
  }

m_macro_like void vector_clear(vector_ptr_t vec) {
  vec->size = 0;
}

#endif
