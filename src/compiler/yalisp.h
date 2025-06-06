#if !defined(H_YALISP)
#define H_YALISP

#include <stdbool.h>

typedef struct {
  const char* const output;
  bool dump_tokens;
} compiler_flags_t;

#endif
