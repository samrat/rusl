#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

int64_t *heap;
int64_t *rootstack;
int64_t *free_ptr;

const int64_t TRUE  = 0xffffffffffffffff;
const int64_t FALSE = 0x7fffffffffffffff;

int rec_print(int64_t val) {
  if(val & 0x00000001 ^ 0x00000001) {
    printf("%" PRId64, val >> 1);
  }
  else if(val == TRUE) {
    printf("#t");
  }
  else if(val == FALSE) {
    printf("#f");
  }
  else if((val & 0x00000003) == 0x00000001) {
    int64_t *tup_base = (int64_t*)(val - 1);
    int tup_count = *tup_base;
    printf("(");
    for (int i = 1; i < tup_count + 1; i++) {
      int64_t ith_val = *(tup_base + i);
      rec_print(ith_val);

      if (i != tup_count) {
        printf(", ");
      }
    }
    printf(")");
  }
  else {
    printf("Unknown value: %#010x", val);
  }
}

int print(int64_t val) {
  rec_print(val);
  printf("\n");
  return val;
}

void initialize() {
  heap = malloc(200);
  rootstack = malloc(200);
  free_ptr = heap;
}
