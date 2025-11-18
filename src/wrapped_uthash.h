#ifndef WRAPPED_UTHASH_H
#define WRAPPED_UTHASH_H

#include "alloc.h"

static inline void* wrapped_alloc(size_t size) {
  return arena_alloc(&long_lived, size);
}

static inline void wrapped_free(void *ptr) {
  // no op
  (void)ptr;
}

#define uthash_malloc(size) wrapped_alloc(size)
#define uthash_free(ptr,sz) wrapped_free(ptr)

#endif
