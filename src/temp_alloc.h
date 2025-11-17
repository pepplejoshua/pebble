#ifndef TEMP_ALLOC_H
#define TEMP_ALLOC_H

#include <stddef.h>

typedef struct TempSlab {
  char *buffer;
  size_t capacity;
  size_t used;
  struct TempSlab *next;
} TempSlab;

typedef struct {
  TempSlab *current;
  TempSlab *first;
  size_t slab_size;
} TempAllocator;

extern TempAllocator temp_allocator;

void temp_init(TempAllocator *temp, size_t initial_capacity);
void *temp_alloc(TempAllocator *temp, size_t size);
void temp_reset(TempAllocator *temp);
void temp_free(TempAllocator *temp);
void temp_get_stats(TempAllocator *temp, size_t *capacity);

#endif
