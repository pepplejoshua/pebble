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

// StackAlloc implementation
void stack_alloc_init(StackAlloc *sa, size_t arena_capacity, size_t scope_capacity) {
    arena_init(&sa->arena, arena_capacity);
    sa->scope_capacity = scope_capacity;
    sa->current = NULL;
    stack_push_scope(sa);  // Start with one scope
}

void stack_push_scope(StackAlloc *sa) {
    StackScope *new_scope = arena_alloc(&sa->arena, sizeof(StackScope));
    new_scope->buffer = arena_alloc(&sa->arena, sa->scope_capacity);
    new_scope->capacity = sa->scope_capacity;
    new_scope->used = 0;
    new_scope->prev = sa->current;
    sa->current = new_scope;
}

void stack_pop_scope(StackAlloc *sa) {
    assert(sa->current && "No scope to pop");
    sa->current = sa->current->prev;
    // Note: We don't free the buffer; it's in the arena, so it stays allocated until stack_alloc_free
}

void *stack_alloc(StackAlloc *sa, size_t size) {
    assert(sa->current && "No current scope");
    StackScope *scope = sa->current;
    if (scope->used + size > scope->capacity) {
        // For simplicity, just assert; in a real impl, you could grow or error
        assert(0 && "Scope capacity exceeded");
    }
    void *ptr = scope->buffer + scope->used;
    scope->used += size;
    return ptr;
}

void stack_alloc_free(StackAlloc *sa) {
  arena_free(&sa->arena);
}
