#include <stdio.h>
#include <inttypes.h>
#include "printer.h"
void printValueHelper(int64_t x)
{
  if ((x & 0x0000000000000001) == 0)
  {
    // Then this is an integer.
    x /= 2;
    printf("%" PRId64, x);
  }
  else if ((x & 0x0000000000000007) == 3)
  {
    // Then this is a channel
    int64_t read_fd = (x & 0xFFFFFFF000000000) >> 36;
    int64_t write_fd = (x & 0x0000000FFFFFFF00) >> 8;
    printf("<channel[R=%" PRId64 ",W=%" PRId64 "]", read_fd, write_fd);
  }
  else if ((x & 0x0000000000000007) == 1)
  {
    // Then this is a heap pointer.
    x &= 0xFFFFFFFFFFFFFFF8;
    int64_t *p = (int64_t *)x;
    int64_t size = (*p) & 0x1FFFFFFFFFFFFFFF;
    int64_t typ = (*p) & 0xE000000000000000;
    if (typ == 0x0000000000000000)
    {
      // Then it points to a tuple.
      printf("(");
      for (int64_t i = 0; i < size; i++)
      {
        if (i != 0)
        {
          printf(", ");
        }
        printValueHelper(p[i + 1]);
      }
      printf(")");
    }
    else if (typ == 0x8000000000000000)
    {
      // Then it points to a closure
      int64_t max = *(p + 1);
      int64_t addr = *(p + 2);
      printf("<closure@%016" PRIx64 ">[%" PRId64 "/%" PRId64 "]", addr, size, max);
      printf("(");
      for (int64_t i = 0; i < max; i++)
      {
        if (i != 0)
        {
          printf(", ");
        }
        if (i < size)
        {
          printValueHelper(p[i + 3]);
        }
        else
        {
          printf("?");
        }
      }
      printf(")");
    }
    else
    {
      // This should never happen, as it means that our heap pointer points to
      // something we don't recognize.
      printf("POINTER TO UNINTERPRETABLE HEAP HEADER: %016" PRIx64, x);
    }
  }
  else if (x == 0x7FFFFFFFFFFFFFFF)
  {
    // Then this is the boolean "false"
    printf("false");
  }
  else if (x == 0xFFFFFFFFFFFFFFFF)
  {
    // Then this is the boolean "true"
    printf("true");
  }
  else
  {
    // This should never happen!  Print the hex of the value so we can debug.
    printf("UNINTERPRETABLE VALUE: %16" PRIx64, x);
  }
}
void printValue(int64_t x)
{
  printValueHelper(x);
  printf("\n");
}