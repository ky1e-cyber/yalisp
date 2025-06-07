#if !defined(H_TABLE)
#define H_TABLE

#include "arena.h"

typedef struct tablenode_t_ {
  const char* key;
  int value;
  struct tablenode_t_* next;
} tablenode_t;

typedef struct {
  arena_ptr_t arena;
  tablenode_t* head;
} table_t;

table_t table_make(arena_ptr_t arena);

void table_release(table_t table);

void table_cleanup(table_t* table);

table_t table_add(table_t table, const char* key, int value);

int table_lookup(table_t table, const char* key);

bool table_contains(table_t table, int value);

#endif
