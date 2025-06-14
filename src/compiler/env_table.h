#if !defined(H_ENV_TABLE)
#define H_ENV_TABLE

#include "arena.h"
#include "shared.h"

typedef struct env_table_node_t_ {
  const char* key;
  name_id_t value;
  struct env_table_node_t_* next;
} env_table_node_t;

typedef struct {
  arena_ptr_t arena;
  env_table_node_t* head;
} env_table_t;

env_table_t env_table_make(arena_ptr_t arena);

env_table_t env_table_add(env_table_t table, const char* key, name_id_t value);

name_id_t env_table_lookup(env_table_t table, const char* key);

bool env_table_contains(env_table_t table, name_id_t value);

#endif
