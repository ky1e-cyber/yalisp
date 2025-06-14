#include <string.h>
#include "arena.h"
#include "env_table.h"
#include "shared.h"

env_table_t env_table_make(arena_ptr_t arena) {
  return (env_table_t){.arena = arena, .head = NULL};
}

env_table_t env_table_add(env_table_t table, const char* key, name_id_t value) {
  env_table_node_t* node =
      (env_table_node_t*)arena_alloc(table.arena, sizeof(env_table_node_t));

  node->key = key;
  node->value = value;
  node->next = table.head;

  table.head = node;

  return table;
}

name_id_t env_table_lookup(env_table_t table, const char* key) {
  env_table_node_t* nxt = table.head;

  while (nxt != NULL) {
    if (strcmp(key, nxt->key) == 0)
      return nxt->value;
    nxt = nxt->next;
  }

  return -1;
}

bool env_table_contains(env_table_t table, name_id_t value) {
  env_table_node_t* nxt = table.head;

  while (nxt != NULL) {
    if (nxt->value == value)
      return true;
    nxt = nxt->next;
  }

  return false;
}
