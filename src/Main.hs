module Main where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import System.Random (StdGen, getStdGen, randomR)


type Inc = Int
type Size = Int -- this may not be the length of the vector !!

fZERO :: (Floating a) => a
fZERO = 0

-- /********************************* BLAS1 routines *************************/
-- 
-- /*     COPIES A VECTOR, X, TO A VECTOR, Y, with the given increments */
-- static void dcopy___(int *n_, const double *dx, int incx, 
-- 		     double *dy, int incy)
-- {
--      int i, n = *n_;
     
--      if (n <= 0) return;
--      if (incx == 1 && incy == 1)
-- 	  memcpy(dy, dx, sizeof(double) * ((unsigned) n));
--      else if (incx == 0 && incy == 1) {
-- 	  double x = dx[0];
-- 	  for (i = 0; i < n; ++i) dy[i] = x;
--      }
--      else {
-- 	  for (i = 0; i < n; ++i) dy[i*incy] = dx[i*incx];
--      }
-- } /* dcopy___ */

dcopyM :: (PrimMonad m, V.Unbox a)
          => Size
          -> V.Vector a
          -> Inc
          -> M.MVector (PrimState m) a
          -> Inc
          -> m ()
dcopyM n _ _ _ _ | n <= 0 = return ()
dcopyM n dx incx dy incy = do
  dx' <- V.thaw dx
  forM_ [0..(n-1)] $ \i -> do
    let 
      iy = i * incy
      ix = i * incx
    dx_ix <- M.read dx' ix
    M.write dy iy dx_ix


dcopy :: V.Unbox a
         => Size
         -> V.Vector a
         -> Inc
         -> V.Vector a
         -> Inc
         -> V.Vector a
dcopy n dx incx dy incy = V.modify modifier dy
  where
    modifier ys = dcopyM n dx incx ys incy

-- /* CONSTANT TIMES A VECTOR PLUS A VECTOR. */
-- static void daxpy_sl__(int *n_, const double *da_, const double *dx, 
-- 		       int incx, double *dy, int incy)
-- {
--      int n = *n_, i;  
--      double da = *da_;

--      if (n <= 0 || da == 0) return;
--      for (i = 0; i < n; ++i) dy[i*incy] += da * dx[i*incx];
-- }

daxpyM :: (PrimMonad m, V.Unbox a, Eq a, Floating a)
          => Size
          -> a
          -> V.Vector a
          -> Inc
          -> M.MVector (PrimState m) a
          -> Inc
          -> m ()
daxpyM n da _ _ _ _ | n <= 0 || da == fZERO = return ()
daxpyM n da dx incx dy incy = do
  dx' <- V.thaw dx
  forM_ [0..(n-1)] $ \i -> do
    let 
      iy = i * incy
      ix = i * incx
    dy_iy <- M.read dy iy
    dx_ix <- M.read dx' ix
    M.write dy iy $ dy_iy + (da * dx_ix)

daxpy :: (V.Unbox a, Eq a, Floating a)
         => Size
         -> a
         -> V.Vector a
         -> Inc
         -> V.Vector a
         -> Inc
         -> V.Vector a
daxpy n da dx incx dy incy = V.modify modifier dy
  where
    modifier ys = daxpyM n da dx incx ys incy

-- /* dot product dx dot dy. */
-- static double ddot_sl__(int *n_, double *dx, int incx, double *dy, int incy)
-- {
--      int n = *n_, i;
--      long double sum = 0;
--      if (n <= 0) return 0;
--      for (i = 0; i < n; ++i) sum += dx[i*incx] * dy[i*incy];
--      return (double) sum;
-- }
ddot :: (V.Unbox a, Eq a, Floating a)
        => Size
        -> V.Vector a
        -> Inc
        -> V.Vector a
        -> Inc
        -> a
ddot n dx incx dy incy
  | n <= 0    = fZERO
  | otherwise = V.foldl' (+) fZERO $ V.zipWith (*) dx' dy'
  where
    dx' = stride n dx incx
    dy' = stride n dy incy

stride :: (V.Unbox a)
          => Size
          -> V.Vector a
          -> Inc
          -> V.Vector a
stride n vec inc = V.take n $ V.ifilter (incrementer inc) vec
  where 
    incrementer inc i _ = 0 == (i `mod` inc)


-- /* compute the L2 norm of array DX of length N, stride INCX */
-- static double dnrm2___(int *n_, double *dx, int incx)
-- {
--      int i, n = *n_;
--      double xmax = 0, scale;
--      long double sum = 0;
--      for (i = 0; i < n; ++i) {
--           double xabs = fabs(dx[incx*i]);
--           if (xmax < xabs) xmax = xabs;
--      }
--      if (xmax == 0) return 0;
--      scale = 1.0 / xmax;
--      for (i = 0; i < n; ++i) {
--           double xs = scale * dx[incx*i];
--           sum += xs * xs;
--      }
--      return xmax * sqrt((double) sum);
-- }
dnrm2 :: (V.Unbox a, Eq a, Ord a, Floating a)
         => Size
         -> V.Vector a
         -> Inc
         -> a
dnrm2 n dx incx =
  let dx' = stride n dx incx
      xmax = V.maximum $ V.map abs dx'
  in 
    case xmax == fZERO of
    True -> fZERO
    False -> sqrt $ ddot (V.length dx') dx' 1 dx' 1


