#if !defined(H_UTILS)
#define H_UTILS

#include <stdio.h>
#include "macros.h"

m_macro_like void file_close_cleanup(FILE** fp) {
  FILE* f = *fp;
  if (f != NULL && f != stdout && f != stdin)
    fclose(f);
}

#endif
