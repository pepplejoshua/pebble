#ifndef LOCATION_H
#define LOCATION_H

// Source location for errors
typedef struct {
  const char *file;
  int line;
  int column;
} Location;

#endif
