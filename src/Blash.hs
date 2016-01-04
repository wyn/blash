{-# LANGUAGE FlexibleContexts #-}

module Blash where

import qualified Data.Vector.Generic as V
import Data.Vector.Generic ((!))
import Data.Complex
import BlashImpl (fZERO, stride, isample, map_reduce, Size, Inc, Index, copyM, axpyM)

main :: IO ()
main = undefined

copy :: (V.Vector v a)
         => Size
         -> v a
         -> Inc
         -> v a
         -> Inc
         -> v a
copy n dx incx dy incy = V.modify modifier dy
  where
    modifier ys = copyM n dx incx ys incy


axpy :: (Ord a, Num a, V.Vector v a)
         => Size
         -> a
         -> v a
         -> Inc
         -> v a
         -> Inc
         -> v a
axpy n da dx incx dy incy = V.modify modifier dy
  where
    modifier ys = axpyM n da dx incx ys incy

-- /* dot product dx dot dy. */
dot :: (Num a, V.Vector v a)
        => Size
        -> v a
        -> Inc
        -> v a
        -> Inc
        -> a
dot n _ _ _ _ | n <= 0 = fZERO
dot n dx incx dy incy = sum prods
  where
    prods = flip map [0..(n-1)] f
    f i = (dx ! ix i) * (dy ! iy i)
    ix = stride n incx
    iy = stride n incy

-- /* dot product dx dot dy where dx & dy are complex. */
dotc :: (RealFloat a, V.Vector v (Complex a))
        => Size
        -> v (Complex a)
        -> Inc
        -> v (Complex a)
        -> Inc
        -> Complex a
dotc n _ _ _ _ | n <= 0 = fZERO :+ fZERO
dotc n dx incx dy incy = sum prods
  where
    prods = flip map [0..(n-1)] f
    f i = (conjugate (dx ! ix i)) * (dy ! iy i)
    ix = stride n incx
    iy = stride n incy
         

-- /* compute the L2 norm of array DX of length N, stride INCX */
-- TODO should really be (Num a) - or split between Complex and Real
nrm2 :: (Ord a, Floating a, V.Vector v a)
        => Size
        -> v a
        -> Inc
        -> a
nrm2 n _  incx | n <= 0 || incx <= 0 = fZERO
nrm2 n dx _ | n == 1 = abs $ dx ! 0
nrm2 n dx incx =
  let dx' = isample n dx incx
      xmax = V.maximum $ V.map abs dx'
  in 
    case xmax == fZERO of
    True -> fZERO
    False ->
      let scale = 1.0 / xmax
          scaled_dx' = V.map (* scale) dx'
          n' = V.length scaled_dx'
          scaled_drnm2 = sqrt $ dot n' scaled_dx' 1 scaled_dx' 1
      in
        xmax * scaled_drnm2

-- DASUM takes the sum of the absolute values.
asum :: (Num a, V.Vector v a)
        => Size
        -> v a
        -> Inc
        -> a
asum = map_reduce abs V.sum         

-- DAMAX takes the max of the absolute values.
iamax :: (Ord a, Num a, V.Vector v a)
        => Size
        -> v a
        -> Inc
        -> Index
iamax = map_reduce abs V.maxIndex
