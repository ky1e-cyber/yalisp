#if !defined(H_ARRAY)
#define H_ARRAY

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include "arena.h"
#include "defs.h"
#include "macros.h"

typedef struct __attribute__((aligned(MAX_ALIGNMENT))) {
  const size_t size;
} array_header_t;

typedef array_header_t* array_ptr_t;

m_macro_like size_t array_bytesize_sized(size_t elem_sz, size_t cnt) {
  return sizeof(array_header_t) + elem_sz * cnt;
}

#define array_bytesize(T, cnt) /* -> size_t */ \
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
    (T*)array_baseptr(arr__);          \
  })

#define array_begin(T, arr) /* -> T* */ \
  ({                                    \
    m_assert_istype(T);                 \
    array_ptr_t arr__ = arr;            \
    (T*)array_baseptr(arr__);           \
  })

#define array_end(T, arr) /* -> T* */     \
  ({                                      \
    m_assert_istype(T);                   \
    array_ptr_t arr__ = arr;              \
    T* data__ = (T*)array_baseptr(arr__); \
    data__ + array_size(arr__);           \
  })

#define array_fill_from(T, arr, buf, buf_sz) /* -> void */ \
  {                                                        \
    m_assert_istype(T);                                    \
    size_t buf_sz__ = buf_sz;                              \
    T* buf__ = buf;                                        \
    array_ptr_t arr__ = arr;                               \
    assert(arr__ != NULL);                                 \
    assert(array_size(arr__) >= buf_sz__);                 \
    for (size_t i = 0; i < array_size(arr__); i++)         \
      ((T*)array_baseptr(arr__))[i] = buf__[i];            \
  }

#define array_foreach(T, arr, op) /* -> void */                \
  {                                                            \
    m_assert_istype(T);                                        \
    array_ptr_t arr__ = arr;                                   \
    void (*op__)(T*) = op;                                     \
    T* begin__ = ((T*)array_baseptr(arr__));                   \
    T* end__ = begin__ + array_size(arr__);                    \
    for (T* it = (T*)array_baseptr(arr__); it < end__; it++) { \
      op__(it);                                                \
    }                                                          \
  }

m_macro_like array_ptr_t array_make_arena_sized(size_t elem_sz,
                                                arena_ptr_t arena,
                                                size_t sz) {
  return array_init(
      (array_ptr_t)arena_alloc(arena, array_bytesize_sized(elem_sz, sz)), sz);
}

#define array_make_arena(T, arena, sz)                \
  ({                                                  \
    m_assert_istype(T);                               \
    arena_ptr_t arena__ = arena;                      \
    size_t sz__ = sz;                                 \
    array_make_arena_sized(sizeof(T), arena__, sz__); \
  })

m_macro_like void array_copy_data_sized(size_t elem_sz,
                                        array_ptr_t arr,
                                        void* dest) {
  size_t bytesz = elem_sz * array_size(arr);
  memcpy(dest, array_baseptr(arr), bytesz);
}

#define array_copy_data(T, arr, dest)                \
  {                                                  \
    m_assert_istype(T);                              \
    array_ptr_t arr__ = arr;                         \
    void* dest__ = dest;                             \
    array_copy_data_sized(sizeof(T), arr__, dest__); \
  }

#endif
