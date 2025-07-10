#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include "error.h"

/*  Runtime‑error codes
      1 – expected int
      2 – expected boolean
      3 – expected tuple
      4 – tuple index out of bounds
      5 – expected closure
      8 – expected channel
      9 – send attempted with non‑int / non‑bool value
*/
void stopWithError(int64_t type)
{
  switch (type)
  {
  case 1:
    printf("Expected an int.\n");
    break;
  case 2:
    printf("Expected a boolean.\n");
    break;
  case 3:
    printf("Expected a tuple.\n");
    break;
  case 4:
    printf("Tuple index out of bounds.\n");
    break;
  case 5:
    printf("Expected a closure.\n");
    break;
  case 8:
    printf("Expected a channel.\n");
    break;
  case 9:
    printf("Only integers or booleans may be sent through a channel.\n");
    break;
  default:
    printf("Unknown error %" PRId64 " occurred.\n", type);
    break;
  }
  /* Use libc exit so stdout is flushed before terminating. */
  exit(type);
}
