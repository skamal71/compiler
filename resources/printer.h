#ifndef _PRINTER_H_
#define _PRINTER_H_

#include <stdint.h>

/**
 * This function interprets a value by examining its bit layout.  It then prints
 * an appropriate representation of it.
 */
void printValue(int64_t x) asm("printValue");

#endif
