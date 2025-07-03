#include <stdio.h>
#include <stdlib.h>

void runtime_check(int cond, const char* msg, const char* file, int line,
                   int col) {
  if (!cond) {
    printf("%s:%d:%d: %s\n", file, line, col, msg);
    exit(1);
  }
}
