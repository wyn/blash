module Blash where

import qualified Data.Vector.Storable as V
import qualified BlashImpl as VI

main :: IO ()
main = undefined

xCopy :: V.Storable a
         => VI.Size
         -> V.Vector a
         -> VI.Inc
         -> V.Vector a
         -> VI.Inc
         -> V.Vector a
xCopy n dx incx dy incy = V.modify modifier dy
  where
    modifier ys = VI.xCopyM n dx incx ys incy


axpy :: (V.Storable a, Eq a, Floating a)
         => VI.Size
         -> a
         -> V.Vector a
         -> VI.Inc
         -> V.Vector a
         -> VI.Inc
         -> V.Vector a
axpy n da dx incx dy incy = V.modify modifier dy
  where
    modifier ys = VI.axpyM n da dx incx ys incy
