#if !defined(H_DEFS)
#define H_DEFS

#include <stdalign.h>
#include <stddef.h>

#define WORDSIZE (sizeof(size_t))
#define MAX_ALIGNMENT (alignof(max_align_t))

#if !defined(VECTOR_INIT_CAPACITY)
#define VECTOR_INIT_CAPACITY 16
#endif

#if !defined(VECTOR_GROW_FACTOR)
#define VECTOR_GROW_FACTOR 2
#endif

#endif
