module BlashImpl where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic ( (!) )
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M


type Inc = Int
type Index = Int
type Size = Int -- this is not the length of the vector !!

fZERO :: (Num a) => a
fZERO = 0

-- /********************************* BLAS1 routines *************************/

-- TODO get the constraint 0 <= i < n in here
stride :: Size -> Inc -> (Int -> Int)
stride n inc =
  -- Old fortran code - need to do
  -- haskell_index = fortran_index-1
  --
  -- IX = 1
  -- IY = 1
  -- IF (INCX.LT.0) IX = (-N+1)*INCX + 1
  -- IF (INCY.LT.0) IY = (-N+1)*INCY + 1
  -- DO I = 1,N
  --    DY(IY) = DX(IX) OR SOME OTHER OPERATION
  --    IX = IX + INCX
  --    IY = IY + INCY
  -- END DO
  let ix = if inc >= 0 then 0 else (1-n)*inc in
  \i -> ix + i * inc


isample :: (V.Vector v a
           )
           => Size
           -> v a
           -> Inc
           -> v a
isample n dx inc = let ix = stride n inc in
  V.fromList $ (flip map) [0..(n-1)] (\i -> dx ! ix i)


-- 
-- /*     SCALES A VECTOR, X, BY A CONSTANT
scalM :: (Num a,
          PrimMonad m,
          M.MVector mv a
         )
         => Size
         -> a
         -> mv (PrimState m) a
         -> Inc
         -> m ()
scalM n _ _ incx | n <= 0 || incx <= 0 = return ()
scalM n da dx incx = do
  let
    ix = stride n incx
  forM_ [0..(n-1)] $ \i -> do
    let ix' = ix i
    dx_ix <- M.read dx ix'
    M.write dx ix' (da * dx_ix)


-- /*     SWAPS VECTOR, X, AND VECTOR, Y, with the given increments */
swapM :: (PrimMonad m,
          M.MVector mv a
         )
          => Size
          -> mv (PrimState m) a
          -> Inc
          -> mv (PrimState m) a
          -> Inc
          -> m ()
swapM n _ _ _ _ | n <= 0 = return ()
swapM n dx incx dy incy = do
  let
    ix = stride n incx
    iy = stride n incy 
  forM_ [0..(n-1)] $ \i -> do
    let ix' = ix i
        iy' = iy i
    dx_ix <- M.read dx ix'
    dy_iy <- M.read dy iy'
    M.write dx ix' dy_iy
    M.write dy iy' dx_ix


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
  dx' <- V.thaw dx
  -- [0..(n-1)] is inclusive of end points
  -- ie [0..(4-1)] == [0,1,2,3]
  let
    ix = stride n incx
    iy = stride n incy 
  forM_ [0..(n-1)] $ \i -> do
    dx_ix <- M.read dx' (ix i)
    M.write dy (iy i) dx_ix


-- /* CONSTANT TIMES A VECTOR PLUS A VECTOR. */
axpyM :: (Ord a,
          Num a,
          PrimMonad m,
          M.MVector mv a,
          V.Vector v a
         )
          => Size
          -> a
          -> v a
          -> Inc
          -> mv (PrimState m) a
          -> Inc
          -> m ()
axpyM n da _ _ _ _ | n <= 0 || da == fZERO = return ()
axpyM n da dx incx dy incy = do
  dx' <- V.thaw dx
  let
    ix = stride n incx
    iy = stride n incy
  forM_ [0..(n-1)] $ \i -> do
    let ix' = ix i
        iy' = iy i
    dy_iy <- M.read dy iy'
    dx_ix <- M.read dx' ix'
    M.write dy iy' (dy_iy + (da * dx_ix))


map_reduce :: (V.Vector v a, V.Vector v b, Num c)
              => (a -> b)
              -> (v b -> c)
              -> Size
              -> v a
              -> Inc
              -> c
map_reduce _ _ n _  incx | n <= 0 || incx <= 0 = fZERO
map_reduce mapper reducer n dx incx =
  let
    dx'  = isample n dx incx
  in
    reducer $ V.map mapper dx'

-- prepare Givens rotation params
rotg :: (Num a, Ord a, Floating a)
        => a
        -> a
        -> (a,a,a,a)
rotg da db = doRotG
  where
    abs_da = abs da
    abs_db = abs db
    scale = abs_da + abs_db
    roe = if abs_da > abs_db then da else db
    doRotG
      | scale == fZERO = (0.0, 0.0, 1.0, 0.0)
      | otherwise      = (da', db', cs', sn')
      where
        r' = (signum roe) * scale * (sqrt $ da2 + db2)
        da2 = (da / scale) ** 2
        db2 = (db / scale) ** 2
        cs' = da / r'
        sn' = db / r'
        da' = r'
        db' = calc_db
        calc_db
          | abs_db >= abs_da && cs' /= fZERO = 1.0 / cs'
          | abs_db < abs_da                  = sn'
          | otherwise                        = 1.0
        
-- /* Apply a plane rotation. */
rotM :: (Ord a,
         Num a,
         PrimMonad m,
         M.MVector mv a
        )
        => Size
        -> mv (PrimState m) a
        -> Inc
        -> mv (PrimState m) a
        -> Inc
        -> a
        -> a
        -> m ()
rotM n _ _ _ _ _ _ | n <= 0 = return ()
rotM n dx incx dy incy c s = do
  let
    ix = stride n incx
    iy = stride n incy
  forM_ [0..(n-1)] $ \i -> do
    let ix' = ix i
        iy' = iy i
    dy_iy <- M.read dy iy'
    dx_ix <- M.read dx ix'
    let tmp = c*dx_ix + s*dy_iy
    M.write dy iy' (c*dy_iy - s*dx_ix)
    M.write dx ix' tmp
    


