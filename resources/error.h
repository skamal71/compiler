#ifndef _ERROR_H_
#define _ERROR_H_

#include <inttypes.h>

void stopWithError(int64_t type) asm("stopWithError");

#endif
