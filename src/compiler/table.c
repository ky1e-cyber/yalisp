#include <string.h>
#include "arena.h"
#include "table.h"

table_t table_make(arena_ptr_t arena) {
  return (table_t){.arena = arena, .head = NULL};
}

void table_release(table_t table) {
  arena_release(table.arena);
}

void table_cleanup(table_t* table) {
  table_release(*table);
}

table_t table_add(table_t table, const char* key, int value) {
  tablenode_t* node =
      (tablenode_t*)arena_alloc(table.arena, sizeof(tablenode_t));

  node->key = key;
  node->value = value;
  node->next = table.head;

  table.head = node;

  return table;
}

int table_lookup(table_t table, const char* key) {
  tablenode_t* nxt = table.head;

  while (nxt != NULL) {
    if (strcmp(key, nxt->key) == 0)
      return nxt->value;
    nxt = nxt->next;
  }

  return -1;
}

bool table_contains(table_t table, int value) {
  tablenode_t* nxt = table.head;

  while (nxt != NULL) {
    if (nxt->value == value)
      return true;
    nxt = nxt->next;
  }

  return false;
}
