#include <stdlib.h>
#include <assert.h>
#include "temp_alloc.h"

void temp_init(TempAllocator *temp, size_t initial_capacity) {
  TempSlab *slab = malloc(sizeof(TempSlab));
  assert(slab && "Temp slab allocation failed");

  slab->buffer = malloc(initial_capacity);
  assert(slab->buffer && "Temp buffer allocation failed");

  slab->capacity = initial_capacity;
  slab->used = 0;
  slab->next = NULL;

  temp->current = slab;
  temp->first = slab;
  temp->slab_size = initial_capacity;
}

static size_t align_up(size_t value, size_t alignment) {
  return (value + alignment - 1) & ~(alignment - 1);
}

void *temp_alloc(TempAllocator *temp, size_t size) {
  const size_t alignment = _Alignof(max_align_t);
  size_t aligned_used = align_up(temp->current->used, alignment);

  // Check if allocation fits in current slab
  if (aligned_used + size <= temp->current->capacity) {
    void *ptr = temp->current->buffer + aligned_used;
    temp->current->used = aligned_used + size;
    return ptr;
  }

  // Current slab is full - try to find an existing slab that fits
  TempSlab *slab = temp->current->next;
  while (slab != NULL) {
    if (size <= slab->capacity) {
      temp->current = slab;
      slab->used = 0;

      void *ptr = slab->buffer;
      slab->used = size;
      return ptr;
    }
    slab = slab->next;
  }

  // No existing slab is big enough - allocate a new one
  size_t new_slab_size = temp->slab_size;
  if (size > new_slab_size) {
    new_slab_size = size + alignment;
  }

  TempSlab *new_slab = malloc(sizeof(TempSlab));
  assert(new_slab && "Temp slab allocation failed");

  new_slab->buffer = malloc(new_slab_size);
  assert(new_slab->buffer && "Temp buffer allocation failed");

  new_slab->capacity = new_slab_size;
  new_slab->used = size;
  new_slab->next = NULL;

  // Link it at the end of the chain
  temp->current->next = new_slab;
  temp->current = new_slab;

  return new_slab->buffer;
}

void temp_reset(TempAllocator *temp) {
  // Reset all slabs' used counters and return to first slab
  TempSlab *slab = temp->first;
  while (slab != NULL) {
    slab->used = 0;
    slab = slab->next;
  }
  temp->current = temp->first;
}

void temp_free(TempAllocator *temp) {
  TempSlab *slab = temp->first;
  while (slab != NULL) {
    TempSlab *next = slab->next;
    free(slab->buffer);
    free(slab);
    slab = next;
  }
  temp->current = NULL;
  temp->first = NULL;
}

void temp_get_stats(TempAllocator *temp, size_t *capacity) {
  *capacity = 0;

  TempSlab *slab = temp->first;
  while (slab != NULL) {
    *capacity += slab->capacity;
    slab = slab->next;
  }
}
