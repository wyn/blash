module Blash where

import qualified Data.Vector.Generic as V
import Data.Vector.Generic ((!))
import qualified BlashImpl as VI

main :: IO ()
main = undefined

copy :: (V.Vector v a)
         => VI.Size
         -> v a
         -> VI.Inc
         -> v a
         -> VI.Inc
         -> v a
copy n dx incx dy incy = V.modify modifier dy
  where
    modifier ys = VI.copyM n dx incx ys incy


axpy :: (V.Vector v a, Eq a, Floating a)
         => VI.Size
         -> a
         -> v a
         -> VI.Inc
         -> v a
         -> VI.Inc
         -> v a
axpy n da dx incx dy incy = V.modify modifier dy
  where
    modifier ys = VI.axpyM n da dx incx ys incy

-- /* dot product dx dot dy. */
dot :: (V.Vector v a, Eq a, Floating a)
        => VI.Size
        -> v a
        -> VI.Inc
        -> v a
        -> VI.Inc
        -> a
dot n _ _ _ _ | n <= 0 = VI.fZERO
dot n dx incx dy incy = sum prods
  where
    prods = flip map [0..(n-1)] f
    f i = dx_ix * dy_iy
      where 
        dx_ix = dx ! ix
        dy_iy = dy ! iy
        ix = ix' + i * incx
        iy = iy' + i * incy
    ix' = if incx >= 0 then 0 else (1-n)*incx
    iy' = if incy >= 0 then 0 else (1-n)*incy
              

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
