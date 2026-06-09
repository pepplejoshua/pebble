#ifndef PROLE_ALLOCATOR_H
#define PROLE_ALLOCATOR_H

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

typedef struct ProleAllocator {
  void *ctx;
  void *(*alloc)(void *ctx, size_t size);
  void *(*realloc)(void *ctx, void *ptr, size_t old_size, size_t new_size);
  void (*free)(void *ctx, void *ptr, size_t size);
} ProleAllocator;

typedef struct ProleArenaSlab {
  char *buffer;
  size_t capacity;
  size_t used;
  struct ProleArenaSlab *next;
} ProleArenaSlab;

typedef struct {
  ProleArenaSlab *current;
  size_t slab_size;
} ProleArena;

typedef struct ProleTrackingAllocation {
  void *ptr;
  size_t size;
  struct ProleTrackingAllocation *next;
} ProleTrackingAllocation;

typedef struct {
  ProleAllocator backing;
  ProleTrackingAllocation *allocations;
  size_t allocation_count;
  size_t bytes_allocated;
  size_t peak_bytes_allocated;
  size_t allocation_events;
  size_t free_events;
  size_t invalid_free_count;
  size_t invalid_realloc_count;
} ProleTrackingAllocator;

ProleAllocator prole_malloc_allocator(void);

void prole_arena_init(ProleArena *arena, size_t initial_capacity);
void prole_arena_free(ProleArena *arena);
ProleAllocator prole_arena_allocator(ProleArena *arena);

void prole_tracking_allocator_init(ProleTrackingAllocator *tracker,
                                   ProleAllocator *backing);
void prole_tracking_allocator_discard_records(ProleTrackingAllocator *tracker);
ProleAllocator prole_tracking_allocator(ProleTrackingAllocator *tracker);
bool prole_tracking_allocator_has_leaks(const ProleTrackingAllocator *tracker);
void prole_tracking_allocator_dump_leaks(const ProleTrackingAllocator *tracker,
                                         FILE *out);

void *prole_alloc(ProleAllocator *allocator, size_t size);
void *prole_realloc(ProleAllocator *allocator, void *ptr, size_t old_size,
                    size_t new_size);
void prole_free(ProleAllocator *allocator, void *ptr, size_t size);
char *prole_strdup(ProleAllocator *allocator, const char *str);

#endif
