{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module VecImpl where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M


type Inc = Int
type Size = Int -- this may not be the length of the vector !!

fZERO :: (Floating a) => a
fZERO = 0

-- /********************************* BLAS1 routines *************************/
-- 
-- /*     COPIES A VECTOR, X, TO A VECTOR, Y, with the given increments */

dcopyM :: (PrimMonad m, V.Storable a)
          => Size
          -> V.Vector a
          -> Inc
          -> M.MVector (PrimState m) a
          -> Inc
          -> m ()
dcopyM n _  incx _  incy | n <= 0 || incx <= 0 || incy <= 0 = return ()
dcopyM n dx incx dy incy = do
  dx' <- V.thaw dx
  forM_ [0..(n-1)] $ \i -> do
    let 
      iy = i * incy
      ix = i * incx
    dx_ix <- M.read dx' ix
    M.write dy iy dx_ix


-- -- /* CONSTANT TIMES A VECTOR PLUS A VECTOR. */

-- daxpyM :: (PrimMonad m, V.Unbox a, Eq a, Floating a)
--           => Size
--           -> a
--           -> V.Vector a
--           -> Inc
--           -> M.MVector (PrimState m) a
--           -> Inc
--           -> m ()
-- daxpyM n da _ _ _ _ | n <= 0 || da == fZERO = return ()
-- daxpyM n da dx incx dy incy = do
--   dx' <- V.thaw dx
--   forM_ [0..(n-1)] $ \i -> do
--     let 
--       iy = i * incy
--       ix = i * incx
--     dy_iy <- M.read dy iy
--     dx_ix <- M.read dx' ix
--     M.write dy iy $ dy_iy + (da * dx_ix)

-- daxpy :: (V.Unbox a, Eq a, Floating a)
--          => Size
--          -> a
--          -> V.Vector a
--          -> Inc
--          -> V.Vector a
--          -> Inc
--          -> V.Vector a
-- daxpy n da dx incx dy incy = V.modify modifier dy
--   where
--     modifier ys = daxpyM n da dx incx ys incy

-- -- /* dot product dx dot dy. */
-- ddot :: (V.Unbox a, Eq a, Floating a)
--         => Size
--         -> V.Vector a
--         -> Inc
--         -> V.Vector a
--         -> Inc
--         -> a
-- ddot n dx incx dy incy
--   | n <= 0    = fZERO
--   | otherwise = V.foldl' (+) fZERO $ V.zipWith (*) dx' dy'
--   where
--     dx' = stride n dx incx
--     dy' = stride n dy incy

-- stride :: (V.Unbox a)
--           => Size
--           -> V.Vector a
--           -> Inc
--           -> V.Vector a
-- stride n vec inc = V.take n $ V.ifilter (incrementer inc) vec
--   where 
--     incrementer inc' i _ = 0 == (i `mod` inc')


-- -- /* compute the L2 norm of array DX of length N, stride INCX */
-- dnrm2 :: (V.Unbox a, Eq a, Ord a, Floating a)
--          => Size
--          -> V.Vector a
--          -> Inc
--          -> a
-- dnrm2 n dx incx =
--   let dx' = stride n dx incx
--       xmax = V.maximum $ V.map abs dx'
--   in 
--     case xmax == fZERO of
--     True -> fZERO
--     False ->
--       let scale = 1.0 / xmax
--           scaled_dx' = V.map (* scale) dx'
--           n' = V.length scaled_dx'
--           scaled_drnm2 = sqrt $ ddot n' scaled_dx' 1 scaled_dx' 1
--       in
--         xmax * scaled_drnm2



