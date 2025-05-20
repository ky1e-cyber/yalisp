#if !defined(H_MEMORY)
#define H_MEMORY

#include <stddef.h>
#include "macros.h"

m_macro_like size_t mem_align_by(size_t x, size_t alignment) {
  return x + (-x & (alignment - 1));
}

#endif
