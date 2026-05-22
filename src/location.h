#ifndef LOCATION_H
#define LOCATION_H

// Source location for errors
#include <stddef.h>

typedef struct {
  const char *file;
  size_t line;
  size_t column;
} Location;

#endif
