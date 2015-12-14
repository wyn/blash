module Vec where

import qualified Data.Vector.Storable as V
import qualified VecImpl as VI

main :: IO ()
main = undefined

dcopy :: V.Storable a
         => VI.Size
         -> V.Vector a
         -> VI.Inc
         -> V.Vector a
         -> VI.Inc
         -> V.Vector a
dcopy n dx incx dy incy = V.modify modifier dy
  where
    modifier ys = VI.dcopyM n dx incx ys incy

