#include <stdlib.h>
#include "allocator.h"

alloc_t alloc_default = {.acquire = malloc,
                         .resize = realloc,
                         .release = free};
