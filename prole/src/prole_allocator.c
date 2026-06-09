#include "../include/prole_allocator.h"

#include <stdlib.h>
#include <string.h>

static void *malloc_alloc(void *ctx, size_t size) {
  (void)ctx;
  return malloc(size);
}

static void *malloc_realloc(void *ctx, void *ptr, size_t old_size,
                            size_t new_size) {
  (void)ctx;
  (void)old_size;
  return realloc(ptr, new_size);
}

static void malloc_free(void *ctx, void *ptr, size_t size) {
  (void)ctx;
  (void)size;
  free(ptr);
}

ProleAllocator prole_malloc_allocator(void) {
  ProleAllocator allocator;
  allocator.ctx = NULL;
  allocator.alloc = malloc_alloc;
  allocator.realloc = malloc_realloc;
  allocator.free = malloc_free;
  return allocator;
}

static size_t align_up(size_t value, size_t alignment) {
  return (value + alignment - 1) & ~(alignment - 1);
}

void prole_arena_init(ProleArena *arena, size_t initial_capacity) {
  arena->current = NULL;
  arena->slab_size = initial_capacity;
}

void prole_arena_free(ProleArena *arena) {
  ProleArenaSlab *slab = arena->current;
  while (slab) {
    ProleArenaSlab *next = slab->next;
    free(slab->buffer);
    free(slab);
    slab = next;
  }

  arena->current = NULL;
  arena->slab_size = 0;
}

static void *arena_alloc(void *ctx, size_t size) {
  ProleArena *arena = ctx;
  const size_t alignment = _Alignof(max_align_t);

  if (size == 0) {
    size = 1;
  }

  if (arena->current) {
    size_t aligned_used = align_up(arena->current->used, alignment);
    if (aligned_used + size <= arena->current->capacity) {
      void *ptr = arena->current->buffer + aligned_used;
      arena->current->used = aligned_used + size;
      return ptr;
    }
  }

  size_t slab_size = arena->slab_size;
  if (slab_size == 0) {
    slab_size = 4096;
  }
  if (size > slab_size) {
    slab_size = size + alignment;
  }

  ProleArenaSlab *slab = malloc(sizeof(ProleArenaSlab));
  if (!slab) {
    return NULL;
  }

  slab->buffer = malloc(slab_size);
  if (!slab->buffer) {
    free(slab);
    return NULL;
  }

  slab->capacity = slab_size;
  slab->used = size;
  slab->next = arena->current;
  arena->current = slab;
  return slab->buffer;
}

static void *arena_realloc(void *ctx, void *ptr, size_t old_size,
                           size_t new_size) {
  void *new_ptr = arena_alloc(ctx, new_size);
  if (!new_ptr) {
    return NULL;
  }

  if (ptr && old_size > 0) {
    size_t copy_size = old_size < new_size ? old_size : new_size;
    memcpy(new_ptr, ptr, copy_size);
  }

  return new_ptr;
}

static void arena_free_block(void *ctx, void *ptr, size_t size) {
  (void)ctx;
  (void)ptr;
  (void)size;
}

ProleAllocator prole_arena_allocator(ProleArena *arena) {
  ProleAllocator allocator;
  allocator.ctx = arena;
  allocator.alloc = arena_alloc;
  allocator.realloc = arena_realloc;
  allocator.free = arena_free_block;
  return allocator;
}

static ProleTrackingAllocation *
tracking_find(ProleTrackingAllocator *tracker, void *ptr,
              ProleTrackingAllocation ***prev_next_out) {
  ProleTrackingAllocation **prev_next = &tracker->allocations;
  ProleTrackingAllocation *current = tracker->allocations;

  while (current) {
    if (current->ptr == ptr) {
      if (prev_next_out) {
        *prev_next_out = prev_next;
      }
      return current;
    }

    prev_next = &current->next;
    current = current->next;
  }

  return NULL;
}

static void tracking_record_alloc(ProleTrackingAllocator *tracker, void *ptr,
                                  size_t size) {
  ProleTrackingAllocation *record = malloc(sizeof(ProleTrackingAllocation));
  if (!record) {
    return;
  }

  record->ptr = ptr;
  record->size = size;
  record->next = tracker->allocations;
  tracker->allocations = record;

  tracker->allocation_count++;
  tracker->bytes_allocated += size;
  if (tracker->bytes_allocated > tracker->peak_bytes_allocated) {
    tracker->peak_bytes_allocated = tracker->bytes_allocated;
  }
}

static void *tracking_alloc(void *ctx, size_t size) {
  ProleTrackingAllocator *tracker = ctx;
  void *ptr = prole_alloc(&tracker->backing, size);
  if (!ptr) {
    return NULL;
  }

  tracker->allocation_events++;
  tracking_record_alloc(tracker, ptr, size == 0 ? 1 : size);
  return ptr;
}

static void *tracking_realloc(void *ctx, void *ptr, size_t old_size,
                              size_t new_size) {
  ProleTrackingAllocator *tracker = ctx;

  if (!ptr) {
    return tracking_alloc(ctx, new_size);
  }

  if (new_size == 0) {
    ProleAllocator allocator = prole_tracking_allocator(tracker);
    prole_free(&allocator, ptr, old_size);
    return NULL;
  }

  ProleTrackingAllocation *record = tracking_find(tracker, ptr, NULL);
  if (!record) {
    tracker->invalid_realloc_count++;
    return NULL;
  }

  void *new_ptr =
      prole_realloc(&tracker->backing, ptr, record->size, new_size);
  if (!new_ptr) {
    return NULL;
  }

  tracker->allocation_events++;
  tracker->bytes_allocated -= record->size;
  tracker->bytes_allocated += new_size;
  if (tracker->bytes_allocated > tracker->peak_bytes_allocated) {
    tracker->peak_bytes_allocated = tracker->bytes_allocated;
  }

  record->ptr = new_ptr;
  record->size = new_size;
  return new_ptr;
}

static void tracking_free(void *ctx, void *ptr, size_t size) {
  (void)size;

  ProleTrackingAllocator *tracker = ctx;
  ProleTrackingAllocation **prev_next = NULL;
  ProleTrackingAllocation *record = tracking_find(tracker, ptr, &prev_next);
  if (!record) {
    tracker->invalid_free_count++;
    return;
  }

  *prev_next = record->next;
  tracker->allocation_count--;
  tracker->bytes_allocated -= record->size;
  tracker->free_events++;

  prole_free(&tracker->backing, ptr, record->size);
  free(record);
}

void prole_tracking_allocator_init(ProleTrackingAllocator *tracker,
                                   ProleAllocator *backing) {
  memset(tracker, 0, sizeof(ProleTrackingAllocator));
  tracker->backing = backing ? *backing : prole_malloc_allocator();
}

void prole_tracking_allocator_discard_records(ProleTrackingAllocator *tracker) {
  ProleTrackingAllocation *current = tracker->allocations;
  while (current) {
    ProleTrackingAllocation *next = current->next;
    free(current);
    current = next;
  }

  tracker->allocations = NULL;
  tracker->allocation_count = 0;
  tracker->bytes_allocated = 0;
}

ProleAllocator prole_tracking_allocator(ProleTrackingAllocator *tracker) {
  ProleAllocator allocator;
  allocator.ctx = tracker;
  allocator.alloc = tracking_alloc;
  allocator.realloc = tracking_realloc;
  allocator.free = tracking_free;
  return allocator;
}

bool prole_tracking_allocator_has_leaks(const ProleTrackingAllocator *tracker) {
  return tracker->allocation_count > 0 || tracker->bytes_allocated > 0 ||
         tracker->invalid_free_count > 0 || tracker->invalid_realloc_count > 0;
}

void prole_tracking_allocator_dump_leaks(const ProleTrackingAllocator *tracker,
                                         FILE *out) {
  fprintf(out,
          "Prole tracking allocator: %zu outstanding allocation(s), %zu "
          "byte(s), %zu invalid free(s), %zu invalid realloc(s)\n",
          tracker->allocation_count, tracker->bytes_allocated,
          tracker->invalid_free_count, tracker->invalid_realloc_count);

  for (ProleTrackingAllocation *record = tracker->allocations; record;
       record = record->next) {
    fprintf(out, "  leak: %p (%zu byte(s))\n", record->ptr, record->size);
  }
}

void *prole_alloc(ProleAllocator *allocator, size_t size) {
  ProleAllocator fallback = prole_malloc_allocator();
  if (!allocator) {
    allocator = &fallback;
  }
  return allocator->alloc(allocator->ctx, size);
}

void *prole_realloc(ProleAllocator *allocator, void *ptr, size_t old_size,
                    size_t new_size) {
  ProleAllocator fallback = prole_malloc_allocator();
  if (!allocator) {
    allocator = &fallback;
  }
  return allocator->realloc(allocator->ctx, ptr, old_size, new_size);
}

void prole_free(ProleAllocator *allocator, void *ptr, size_t size) {
  if (!ptr) {
    return;
  }

  ProleAllocator fallback = prole_malloc_allocator();
  if (!allocator) {
    allocator = &fallback;
  }
  allocator->free(allocator->ctx, ptr, size);
}

char *prole_strdup(ProleAllocator *allocator, const char *str) {
  if (!str) {
    return NULL;
  }

  size_t len = strlen(str) + 1;
  char *copy = prole_alloc(allocator, len);
  if (!copy) {
    return NULL;
  }

  memcpy(copy, str, len);
  return copy;
}
