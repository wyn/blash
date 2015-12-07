
#include <math.h>

#include <string.h>

void inline_c_Main_0_e208c1aac139e6f3558f013e9029f303672b1f82(int n__inline_c_0, int incx_inline_c_1, int incy_inline_c_2, double * dy_inline_c_3, double * dx_inline_c_4, int incx_inline_c_5, int incy_inline_c_6, double * dx_inline_c_7, double * dy_inline_c_8, double * dy_inline_c_9, int incy_inline_c_10, double * dx_inline_c_11, int incx_inline_c_12) {

     int i = 0;
     int n = n__inline_c_0;
     if (n <= 0) return;
     if (incx_inline_c_1 == 1 && incy_inline_c_2 == 1)
          memcpy(dy_inline_c_3, dx_inline_c_4, sizeof(double) * ((unsigned) n));
     else if (incx_inline_c_5 == 0 && incy_inline_c_6 == 1) {
          double x = dx_inline_c_7[0];
          for (i = 0; i < n; ++i) dy_inline_c_8[i] = x;
     }
     else {
          for (i = 0; i < n; ++i) dy_inline_c_9[i*incy_inline_c_10] = dx_inline_c_11[i*incx_inline_c_12];
     }
   
}

