#ifndef ALLOC_H
#define ALLOC_H

#include <stddef.h>

// Long-lived arena allocator
typedef struct {
    char *buffer;
    size_t capacity;
    size_t used;
} Arena;

void arena_init(Arena *arena, size_t initial_capacity);
void *arena_alloc(Arena *arena, size_t size);
void arena_free(Arena *arena);  // Frees the whole arena

// Ephemeral stack allocator
typedef struct StackScope {
    char *buffer;
    size_t capacity;
    size_t used;
    struct StackScope *prev;  // Stack link
} StackScope;

typedef struct {
    StackScope *current;
    Arena arena;  // Use the long-lived arena to allocate scope buffers
    size_t scope_capacity;
} StackAlloc;

void stack_alloc_init(StackAlloc *sa, size_t arena_capacity, size_t scope_capacity);
void stack_push_scope(StackAlloc *sa);
void stack_pop_scope(StackAlloc *sa);
void *stack_alloc(StackAlloc *sa, size_t size);
void stack_alloc_free(StackAlloc *sa);

#endif
