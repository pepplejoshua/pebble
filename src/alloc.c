#include "alloc.h"
#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Arena implementation
void arena_init(Arena *arena, size_t initial_capacity) {
  // Create first slab
  Slab *slab = malloc(sizeof(Slab));
  assert(slab && "Slab allocation failed");

  slab->buffer = malloc(initial_capacity);
  assert(slab->buffer && "Slab buffer allocation failed");
  memset(slab->buffer, 0, initial_capacity);

  slab->capacity = initial_capacity;
  slab->used = 0;
  slab->next = NULL;

  // Initialize arena
  arena->current = slab;
  arena->slab_size = initial_capacity;
}

// Helper: align a value up to the given alignment
static size_t align_up(size_t value, size_t alignment) {
  return (value + alignment - 1) & ~(alignment - 1);
}

void *arena_alloc(Arena *arena, size_t size) {
  // Alignment requirement (typically 8 or 16 bytes)
  const size_t alignment = _Alignof(max_align_t);

  // Align the current position
  size_t aligned_used = align_up(arena->current->used, alignment);

  // Check if allocation fits in current slab
  if (aligned_used + size <= arena->current->capacity) {
    void *ptr = arena->current->buffer + aligned_used;
    arena->current->used = aligned_used + size;
    return ptr;
  }

  // Doesn't fit - need a new slab
  // Decide slab size: use default or custom size for large allocations
  size_t new_slab_size = arena->slab_size;
  if (size > new_slab_size) {
    new_slab_size = size + alignment; // Custom size for large allocation
  }

  // Allocate new slab
  Slab *new_slab = malloc(sizeof(Slab));
  assert(new_slab && "Slab allocation failed");

  new_slab->buffer = malloc(new_slab_size);
  assert(new_slab->buffer && "Slab buffer allocation failed");
  memset(new_slab->buffer, 0, new_slab_size);

  new_slab->capacity = new_slab_size;
  new_slab->used = 0;
  new_slab->next = arena->current;

  // Link new slab and make it current
  arena->current = new_slab;

  // Allocate from new slab (it's empty so alignment is already satisfied)
  void *ptr = new_slab->buffer;
  new_slab->used = size;
  return ptr;
}

void arena_free(Arena *arena) {
  Slab *current = arena->current;

  // Walk the linked list and free each slab
  while (current != NULL) {
    Slab *next = current->next;
    free(current->buffer); // Free the slab's buffer
    free(current);         // Free the slab struct itself
    current = next;
  }

  // Reset arena to empty state
  arena->current = NULL;
  arena->slab_size = 0;
}

void arena_get_stats(Arena *arena, size_t *used, size_t *capacity) {
  *used = 0;
  *capacity = 0;

  Slab *slab = arena->current;
  while (slab != NULL) {
    *used += slab->used;
    *capacity += slab->capacity;
    slab = slab->next;
  }
}

// Duplicate string to arena
char *str_dup(const char *str) {
  if (!str)
    return NULL;
  size_t len = strlen(str) + 1; // Include null terminator
  char *copy = arena_alloc(&long_lived, len);
  memcpy(copy, str, len); // Copies the '\0' too
  return copy;
}
