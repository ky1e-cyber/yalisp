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

#if !defined(PT_ARENA_SIZE)
#define PT_ARENA_SIZE (1 << 20)
#endif

#if !defined(ENV_ARENA_SIZE)
#define ENV_ARENA_SIZE (1 << 20)
#endif

#if !defined(PARSER_GLOBALS_ARENA_SIZE)
#define GLOBALS_ARENA_SIZE (1 << 20)
#endif

#if !defined(STR_ARENA_SIZE)
#define STR_ARENA_SIZE (1 << 20)
#endif

#endif
