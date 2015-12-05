{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.C.Inline as C
-- import Control.Monad (forM_)
-- import Control.Monad.Primitive (PrimMonad, PrimState)
-- import qualified Data.Vector.Unboxed as V
-- import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Foreign (alloca)
import           Foreign.C.Types
import           Foreign.Ptr (Ptr)
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (Storable) --, peek)
import qualified Test.Hspec as Hspec

C.include "<math.h>"
C.include "<string.h>"

vectorFromC :: Storable a => C.CInt -> Ptr a -> IO (VS.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  VS.freeze $ VSM.unsafeFromForeignPtr0 ptr' $ fromIntegral len

vectorToC :: Storable a => VS.Vector a -> C.CInt -> Ptr a -> IO ()
vectorToC vec neq ptr = do
  ptr' <- newForeignPtr_ ptr
  VS.copy (VSM.unsafeFromForeignPtr0 ptr' $ fromIntegral neq) vec


main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "Passing to inline-c" $ do
    Hspec.it "Call simple function with c-array" $ do
      let xs = VS.fromList [1,2,3] :: VS.Vector C.CInt
      x <- alloca $ \ptr -> do
        vectorToC xs 3 ptr
        main_ ptr
      x `Hspec.shouldBe` -0.4161468365471424

main_ :: Ptr CInt -> IO (CDouble)
main_ n = do
  [C.block| double {
       int nn = $(int* n)[1];
       return cos(nn);
       }
  |]

         
-- /********************************* BLAS1 routines *************************/
-- 
-- /*     COPIES A VECTOR, X, TO A VECTOR, Y, with the given increments */
dcopy___ :: Ptr CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO ()
dcopy___ n_ dx incx dy incy = do
  [C.block| void
   {
       int i, n = *$(int* n_);
     
       if (n <= 0) return;
       if ($(int incx) == 1 && $(int incy) == 1)
	    memcpy($(double* dy), $(double* dx), sizeof(double) * ((unsigned) n));
       else if ($(int incx) == 0 && $(int incy) == 1) {
	    double x = $(double* dx)[0];
	    for (i = 0; i < n; ++i) $(double* dy)[i] = x;
       }
       else {
	    for (i = 0; i < n; ++i) $(double* dy)[i*$(int incy)] = $(double* dx)[i*$(int incx)];
       }
       
   }
   |]
  

-- -- /* CONSTANT TIMES A VECTOR PLUS A VECTOR. */
-- -- static void daxpy_sl__(int *n_, const double *da_, const double *dx, 
-- -- 		       int incx, double *dy, int incy)
-- -- {
-- --      int n = *n_, i;  
-- --      double da = *da_;

-- --      if (n <= 0 || da == 0) return;
-- --      for (i = 0; i < n; ++i) dy[i*incy] += da * dx[i*incx];
-- -- }


-- -- /* dot product dx dot dy. */
-- -- static double ddot_sl__(int *n_, double *dx, int incx, double *dy, int incy)
-- -- {
-- --      int n = *n_, i;
-- --      long double sum = 0;
-- --      if (n <= 0) return 0;
-- --      for (i = 0; i < n; ++i) sum += dx[i*incx] * dy[i*incy];
-- --      return (double) sum;
-- -- }

-- -- /* compute the L2 norm of array DX of length N, stride INCX */
-- -- static double dnrm2___(int *n_, double *dx, int incx)
-- -- {
-- --      int i, n = *n_;
-- --      double xmax = 0, scale;
-- --      long double sum = 0;
-- --      for (i = 0; i < n; ++i) {
-- --           double xabs = fabs(dx[incx*i]);
-- --           if (xmax < xabs) xmax = xabs;
-- --      }
-- --      if (xmax == 0) return 0;
-- --      scale = 1.0 / xmax;
-- --      for (i = 0; i < n; ++i) {
-- --           double xs = scale * dx[incx*i];
-- --           sum += xs * xs;
-- --      }
-- --      return xmax * sqrt((double) sum);
-- -- }


