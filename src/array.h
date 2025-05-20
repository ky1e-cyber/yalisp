#if !defined(H_ARRAY)
#define H_ARRAY

#include <stdbool.h>
#include <stddef.h>

#include <string.h>

#include "defs.h"
#include "macros.h"

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  const size_t size;
} array_header_t;

typedef array_header_t* array_ptr_t;

m_macro_like size_t array_bytesize_sized(size_t elem_sz, size_t cnt) {
  return sizeof(array_header_t) + elem_sz * cnt;
}

#define arary_bytesize(T, cnt) /* -> size_t */ \
  ({                                           \
    m_assert_istype(T);                        \
    size_t cnt__ = cnt;                        \
    array_bytesize_sized(sizeof(T), cnt__);    \
  })

m_macro_like array_ptr_t array_init(array_ptr_t arr, size_t sz) {
  array_header_t arr_hd = {.size = sz};
  memcpy(arr, &arr_hd, sizeof(array_header_t));
  return arr;
}

m_macro_like size_t array_size(array_ptr_t arr) {
  return arr->size;
}

m_macro_like void* array_baseptr(array_ptr_t arr) {
  return (void*)(arr + 1);
}

#define array_data(T, arr) /* -> T* */ \
  ({                                   \
    m_assert_istype(T);                \
    array_ptr_t arr__ = arr;           \
    (T*)array_baseptr(arr);            \
  })

#define array_begin(T, arr) /* -> T* */ \
  ({                                    \
    m_assert_istype(T);                 \
    array_ptr_t arr__ = arr;            \
    array_data(T, arr);                 \
  })

#define array_end(T, arr) /* -> const T* */ \
  ({                                        \
    m_assert_istype(T);                     \
    array_ptr_t arr__ = arr;                \
    const T* data__ = array_data(T, arr__); \
    (const T*)(data__ + array_size(arr__)); \
  })

#define array_fill_from(T, arr, buf, buf_sz) /* -> void */ \
  {                                                        \
    m_assert_istype(T);                                    \
    size_t buf_sz__ = buf_sz;                              \
    const T* const buf__ = buf;                            \
    array_ptr_t arr__ = arr;                               \
    assert(arr__ != NULL);                                 \
    assert(array_size(arr__) >= buf_sz__);                 \
    const T* end__ = array_end(T, arr__);                  \
    for (T* it = array_begin(T, arr__); it < end__; it++)  \
      it = buf__[i];                                       \
  }

#define array_foreach(T, arr, op) /* -> void */             \
  {                                                         \
    m_assert_istype(T);                                     \
    arr_ptr_t arr__ = arr;                                  \
    void (*const op__)(T*) = op;                            \
    const T* end__ = array_end(T, arr__);                   \
    for (T* it = array_begin(T, vec__); it < end__; it++) { \
      op__(it);                                             \
    }                                                       \
  }

#endif
