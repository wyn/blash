{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module BlashImpl where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M


type Inc = Int
type Size = Int -- this may not be the length of the vector !!

fZERO :: (Floating a) => a
fZERO = 0

-- /********************************* BLAS1 routines *************************/
-- 
-- /*     COPIES A VECTOR, X, TO A VECTOR, Y, with the given increments */
copyM :: (PrimMonad m,
          M.MVector mv a,
          V.Vector v a
         )
          => Size
          -> v a
          -> Inc
          -> mv (PrimState m) a
          -> Inc
          -> m ()
copyM n _ _ _ _ | n <= 0 = return ()
copyM n dx incx dy incy = do
  -- Old fortran code - need to do
  -- haskell_index = fortran_index-1
  --
  -- IX = 1
  -- IY = 1
  -- IF (INCX.LT.0) IX = (-N+1)*INCX + 1
  -- IF (INCY.LT.0) IY = (-N+1)*INCY + 1
  -- DO I = 1,N
  --    DY(IY) = DX(IX)
  --    IX = IX + INCX
  --    IY = IY + INCY
  -- END DO
  dx' <- V.thaw dx
  -- [0..(n-1)] is inclusive of end points
  -- ie [0..(4-1)] == [0,1,2,3]
  let
    ix' = if incx >= 0 then 0 else (1-n)*incx
    iy' = if incy >= 0 then 0 else (1-n)*incy
  forM_ [0..(n-1)] $ \i -> do
    let 
      ix = ix' + i * incx
      iy = iy' + i * incy
    dx_ix <- M.read dx' ix
    M.write dy iy dx_ix


-- /* CONSTANT TIMES A VECTOR PLUS A VECTOR. */
axpyM :: (PrimMonad m,
          M.MVector mv a,
          V.Vector v a,
          Floating a,
          Eq a
         )
          => Size
          -> a
          -> v a
          -> Inc
          -> mv (PrimState m) a
          -> Inc
          -> m ()
axpyM  n _ _ _ _ _ | n <= 0 = return ()
axpyM n da dx incx dy incy = do
  dx' <- V.thaw dx
  let
    ix' = if incx >= 0 then 0 else (1-n)*incx
    iy' = if incy >= 0 then 0 else (1-n)*incy
  forM_ [0..(n-1)] $ \i -> do
    let 
      ix = ix' + i * incx
      iy = iy' + i * incy
    dy_iy <- M.read dy iy
    dx_ix <- M.read dx' ix
    M.write dy iy $ dy_iy + (da * dx_ix)




