#if !defined(H_GLOBALS_TABLE)
#define H_GLOBALS_TABLE

#include "arena.h"

typedef struct globals_table_node_t_ {
  const char* value;
  struct globals_table_node_t_* next;
} globals_table_node_t;

typedef struct {
  arena_ptr_t arena;
  globals_table_node_t* head;
} globals_table_t;

globals_table_t globals_table_make(arena_ptr_t arena);

void globals_table_release(globals_table_t table);

void globals_table_cleanup(globals_table_t* table);

globals_table_t globals_table_add(globals_table_t table, const char* value);

bool globals_table_contains(globals_table_t table, const char* value);

#endif
