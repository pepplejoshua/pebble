#include "alloc.h"
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

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
  arena->free_head = NULL;
  arena->free_current = NULL;
}

// Helper: align a value up to the given alignment
static size_t align_up(size_t value, size_t alignment) {
  return (value + alignment - 1) & ~(alignment - 1);
}

static void relink_slot(Arena *arena, FreeSlot *current) {
  FreeSlot *next = current->next;
  FreeSlot *prev = current->prev;

  free(current);

  if (prev != NULL) {
    // Link last slots next as this slots next
    prev->next = next;
  } else {
    // This is head, we can clear
    arena->free_head = NULL;
    arena->free_current = NULL;
  }
}

static void add_free_slot(Arena *arena, FreeSlot *slot) {
  if (arena->free_head == NULL) {
    arena->free_head = slot;
    arena->free_current = slot;
  } else {
    slot->prev = arena->free_current;
    arena->free_current->next = slot;
    arena->free_current = arena->free_current->next;
  }
}

void *arena_alloc(Arena *arena, size_t size) {
  assert(arena && arena->current && arena->current->buffer);

  const size_t alignment = _Alignof(max_align_t);
  size_t aligned_used = align_up(arena->current->used, alignment);
  size_t aligned_size = align_up(sizeof(MemHeader) + size, alignment);

  if (aligned_used + aligned_size <= arena->current->capacity) {
    char *ptr = arena->current->buffer + aligned_used;

    MemHeader *header = (MemHeader *)ptr;
    header->size = aligned_size;

    arena->current->used = aligned_used + aligned_size;

    return ptr + sizeof(MemHeader);
  }

  // Doesn't fit - need a new slab
  // Decide slab size: use default or custom size for large allocations
  size_t new_slab_size = arena->slab_size;
  if (aligned_size > new_slab_size) {
    new_slab_size = align_up(aligned_size + alignment, alignment); // Custom size for large allocation
  }

  // Allocate new slab
  Slab *new_slab = malloc(sizeof(Slab));
  assert(new_slab && "Slab allocation failed");

  new_slab->buffer = malloc(new_slab_size);
  assert(new_slab->buffer && "Slab buffer allocation failed");
  memset(new_slab->buffer, 0, new_slab_size);

  new_slab->capacity = new_slab_size;
  new_slab->used = aligned_size;
  new_slab->next = arena->current;

  // Link new slab and make it current
  arena->current = new_slab;

  // Allocate from new slab (it's empty so alignment is already satisfied)
  char *ptr = new_slab->buffer;
  MemHeader *header = (MemHeader *)ptr;
  header->size = aligned_size;

  return ptr + sizeof(MemHeader);
}

void *arena_realloc(Arena *arena, void *data, size_t new_size) {
  // Check if address of end of allocation is identical to end of used arena buffer
  if (data != NULL) {
    MemHeader *header = (data - sizeof(MemHeader));
    bool address_is_end = (data + header->size) == (arena->current->buffer + arena->current->used);

    if (!address_is_end && new_size < header->size) {
      // We're shrinking between allocations
      size_t size_diff = header->size - new_size;
      header->size = new_size;

      // We can re-use the gap between the data end and next allocation
      FreeSlot *slot = malloc(sizeof(FreeSlot));
      slot->ptr = data + header->size;
      slot->size = size_diff;

      add_free_slot(arena, slot);

      return data;
    } else if (address_is_end) {
      // There hasn't been any allocations, we can try and resize in place
      if (new_size > header->size) {
        // We're expanding - see if we can fit in slab
        const size_t alignment = _Alignof(max_align_t);

        // Check aligned size as if it were a new allocation to get accurate size
        size_t aligned_used = align_up(arena->current->used - (header->size + sizeof(MemHeader)), alignment);

        size_t total_size = new_size + sizeof(MemHeader);

        // Check new size is capable of fitting in current slab
        if (aligned_used + total_size <= arena->current->capacity) {
          arena->current->used = aligned_used + total_size;

          header->size = new_size;

          return data;
        }
      } else {
        // We're shrinking - we can just decrement buffer used and size in header
        size_t size_diff = header->size - new_size;
        arena->current->used -= size_diff;
        header->size = new_size;

        return data;
      }
    }

    // We can now re-use this area of memory
    FreeSlot *slot = malloc(sizeof(FreeSlot));
    slot->ptr = (char *)header;
    slot->size = header->size;

    add_free_slot(arena, slot);

    // Allocate new buffer and copy data from old allocation
    void *new_data = arena_alloc(arena, new_size);
    memcpy(new_data, data, header->size);
    return new_data;
  }

  // Allocate new buffer (no buffer to copy)
  return arena_alloc(arena, new_size);
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

  // Free the free list
  if (arena->free_head != NULL) {
    FreeSlot *current_free = arena->free_head;

    while (current_free != NULL) {
      FreeSlot *next = current_free->next;
      free(current_free);
      current_free = next;
    }
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
  size_t len = strlen(str); // Include null terminator
  char *copy = arena_alloc(&long_lived, len + 1);
  memcpy(copy, str, len); // Copies the '\0' too
  copy[len] = '\0';
  return copy;
}
