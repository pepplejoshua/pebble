#include "alloc.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Arena implementation
void arena_init(Arena *arena, size_t initial_capacity) {
    arena->buffer = malloc(initial_capacity);
    assert(arena->buffer && "Arena malloc failed");
    arena->capacity = initial_capacity;
    arena->used = 0;
}

void *arena_alloc(Arena *arena, size_t size) {
    if (arena->used + size > arena->capacity) {
        // Simple grow: double capacity
        size_t new_capacity = arena->capacity * 2;
        char *new_buffer = realloc(arena->buffer, new_capacity);
        assert(new_buffer && "Arena realloc failed");
        arena->buffer = new_buffer;
        arena->capacity = new_capacity;
    }
    void *ptr = arena->buffer + arena->used;
    arena->used += size;
    return ptr;
}

void arena_free(Arena *arena) {
    free(arena->buffer);
    arena->buffer = NULL;
    arena->capacity = 0;
    arena->used = 0;
}
