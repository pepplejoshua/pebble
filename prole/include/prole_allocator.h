#ifndef PROLE_ALLOCATOR_H
#define PROLE_ALLOCATOR_H

#include <stddef.h>

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

ProleAllocator prole_malloc_allocator(void);

void prole_arena_init(ProleArena *arena, size_t initial_capacity);
void prole_arena_free(ProleArena *arena);
ProleAllocator prole_arena_allocator(ProleArena *arena);

void *prole_alloc(ProleAllocator *allocator, size_t size);
void *prole_realloc(ProleAllocator *allocator, void *ptr, size_t old_size,
                    size_t new_size);
void prole_free(ProleAllocator *allocator, void *ptr, size_t size);
char *prole_strdup(ProleAllocator *allocator, const char *str);

#endif
