#include "atest.h"

int readAndSum(int n) {
  // Read and sum n integers
  int i, sum = 0;
  for (i = 0; i < n; i++) {
    sum += i;
  }
  return sum;
}
