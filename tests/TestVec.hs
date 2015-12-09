{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.C.Inline as C
-- import Control.Monad (forM_)
-- import Control.Monad.Primitive (PrimMonad, PrimState)
-- import qualified Data.Vector.Unboxed as V
-- import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Storable as VS
-- import qualified Data.Vector.Storable.Mutable as VSM
-- import           Foreign (alloca)
import           Foreign.C.Types
-- import           Foreign.Ptr (Ptr)
-- import           Foreign.ForeignPtr (newForeignPtr_)
-- import           Foreign.Storable (Storable) --, peek)
import           Data.Monoid ((<>))
import qualified Test.Hspec as Hspec

import qualified Vec as M


C.context (C.baseCtx <> C.vecCtx)

C.include "<math.h>"
C.include "<string.h>"


main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "dcopy haskell implementation" $ do
    -- setup TODO do as quickcheck gens
    let xs_ = [1,2,3,4,5,6]
        ys_ = [6,7,8,9,1,2,3,4,5]
        (n, incx, incy) = (2, 2, 3)
        
    Hspec.it "dcopy should compare exactly to inline-c dcopy___W" $ do
      let expected = VS.fromList ys_
      dcopy___W n (VS.fromList xs_) incx expected incy

      -- logic starts here
      let actual = M.dcopy (fromIntegral n) (VS.fromList xs_) (fromIntegral incx) (VS.fromList ys_) (fromIntegral incy)

      -- invariant: same size as ys always      
      length ys_ `Hspec.shouldBe` VS.length actual
      length ys_ `Hspec.shouldBe` VS.length expected

      -- invariant: both methods give same answer
      actual `Hspec.shouldBe` expected
      
    Hspec.it "dcopyM should compare exactly to inline-c dcopy___W" $ do
      let expected = VS.fromList ys_
      dcopy___W n (VS.fromList xs_) incx expected incy

      -- logic starts here
      actual' <- VS.thaw $ VS.fromList ys_
      M.dcopyM (fromIntegral n) (VS.fromList xs_) (fromIntegral incx) actual' (fromIntegral incy)
      actual <- VS.freeze actual'

      -- invariant: same size as ys always
      length ys_ `Hspec.shouldBe` VS.length actual
      length ys_ `Hspec.shouldBe` VS.length expected

      -- invariant: both methods give same answer
      actual `Hspec.shouldBe` expected


         
-- /********************************* BLAS1 routines *************************/
-- 
-- /*     COPIES A VECTOR, X, TO A VECTOR, Y, with the given increments */
dcopy___W :: CInt -> VS.Vector CDouble -> CInt -> VS.Vector CDouble -> CInt -> IO ()
dcopy___W n_ dx incx dy incy = do
  [C.block| void
   {
     int i = 0;
     int n = $(int n_);
     if (n <= 0) return;
     if ($(int incx) == 1 && $(int incy) == 1)
          memcpy($vec-ptr:(double* dy), $vec-ptr:(double* dx), sizeof(double) * ((unsigned) n));
     else if ($(int incx) == 0 && $(int incy) == 1) {
          double x = $vec-ptr:(double* dx)[0];
          for (i = 0; i < n; ++i) $vec-ptr:(double* dy)[i] = x;
     }
     else {
          for (i = 0; i < n; ++i) $vec-ptr:(double* dy)[i*$(int incy)] = $vec-ptr:(double* dx)[i*$(int incx)];
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


