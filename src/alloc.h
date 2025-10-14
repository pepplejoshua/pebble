#ifndef ALLOC_H
#define ALLOC_H

#include <stddef.h>

// Long-lived arena allocator
typedef struct {
    char *buffer;
    size_t capacity;
    size_t used;
} Arena;

extern Arena long_lived;

void arena_init(Arena *arena, size_t initial_capacity);
void *arena_alloc(Arena *arena, size_t size);
void arena_free(Arena *arena);  // Frees the whole arena

// Utility functions
char *str_dup(const char *str);  // Duplicate string to arena

#endif
