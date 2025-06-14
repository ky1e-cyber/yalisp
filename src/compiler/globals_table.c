#include <string.h>
#include "arena.h"
#include "globals_table.h"

globals_table_t globals_table_make(arena_ptr_t arena) {
  return (globals_table_t){.arena = arena, .head = NULL};
}

globals_table_t globals_table_add(globals_table_t table, const char* value) {
  globals_table_node_t* node = (globals_table_node_t*)arena_alloc(
      table.arena, sizeof(globals_table_node_t));

  node->value = value;
  node->next = table.head;

  table.head = node;

  return table;
}

bool globals_table_contains(globals_table_t table, const char* value) {
  globals_table_node_t* nxt = table.head;

  while (nxt != NULL) {
    if (strcmp(nxt->value, value) == 0)
      return true;
    nxt = nxt->next;
  }

  return false;
}
