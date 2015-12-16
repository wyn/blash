module Blash where

import qualified Data.Vector.Generic as V
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
