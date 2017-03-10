#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

int64_t *heap;
int64_t *rootstack;
int64_t *free_ptr;

// print an integer to stdout
void print_int(int64_t x) {
  printf("%" PRId64, x);
}

void initialize() {
  heap = malloc(200);
  rootstack = malloc(200);
  free_ptr = heap;
}
