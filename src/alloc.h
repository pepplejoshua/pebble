#ifndef ALLOC_H
#define ALLOC_H
#include <stddef.h>

// Single slab of memory in the arena
typedef struct Slab {
  char *buffer;
  size_t capacity;
  size_t used;
  struct Slab *next;
} Slab;

// Arena structure
typedef struct Arena {
  Slab *current;    // Current slab we're allocating from
  size_t slab_size; // Default size for new slabs
} Arena;

extern Arena long_lived;

void arena_init(Arena *arena, size_t initial_capacity);
void *arena_alloc(Arena *arena, size_t size);
void arena_free(Arena *arena); // Frees the whole arena
void arena_get_stats(Arena *arena, size_t *used, size_t *capacity);

// Utility functions
char *str_dup(const char *str); // Duplicate string to arena

#endif
