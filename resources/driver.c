#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include "printer.h"

extern int64_t bird_main(int64_t* heap_ptr) asm("bird_main");

int main(int argc, char** argv) {
  int64_t heap_size = 1000000;
  int64_t* heap = malloc(sizeof(int64_t) * heap_size);

  if (heap == NULL) {
      fprintf(stderr, "Failed to allocate heap memory\n");
      return 1;
  }

  int64_t result = bird_main(heap);
  printValue(result);

  free(heap);

  return 0;
}
