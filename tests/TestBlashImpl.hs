module TestBlashImpl where

import           Data.Coerce (coerce)
import qualified Data.Vector.Storable as VS
import           Foreign.C.Types
import           Test.QuickCheck (Positive(..), NonZero(..))
import           Test.QuickCheck.Property (Property)
import           Test.QuickCheck.Monadic
import           Data.AEq 
import qualified BlashImpl as BI  
import           OpenBLAS1  
import           TestBlashData


prop_copyM :: BlasArgs Double -> Property
prop_copyM (BlasArgs (Positive n) xs (NonZero incx) ys (NonZero incy)) = monadicIO $ do
  -- expected uses the CBLAS implementation via inline-c
  expected <- run $ do
    let expected' = VS.fromList (coerce ys)
        xs' = VS.fromList (coerce xs)
        n' = fromIntegral n
        incx' = fromIntegral incx
        incy' = fromIntegral incy
    cblas_copyW n' xs' incx' expected' incy'
    return expected'
    
  -- actual calls the monadic haskell implementation directly
  actual <- run $ do
    let xs' = VS.fromList xs
    actual' <- VS.thaw $ VS.fromList ys
    BI.copyM n xs' incx actual' incy
    VS.freeze actual'

  -- invariant: same size as ys always
  assert (length ys == VS.length actual)
  assert (length ys == VS.length expected)

  -- invariant: both methods give same answer
  let ass = VS.toList actual
      ess = VS.toList expected
  assert $ and $ zipWith (\a e -> a ~== coerce e) ass ess
  

prop_axpyM :: BlasArgs Double -> Double -> Property
prop_axpyM (BlasArgs (Positive n) xs (NonZero incx) ys (NonZero incy)) da = monadicIO $ do
  -- expected uses the CBLAS implementation via inline-c
  expected <- run $ do
    let expected' = VS.fromList (coerce ys)
        xs' = VS.fromList (coerce xs)
        n' = fromIntegral n
        incx' = fromIntegral incx
        incy' = fromIntegral incy
        da' = coerce da
    cblas_axpyW n' da' xs' incx' expected' incy'
    return expected'
    
  -- actual calls the monadic haskell implementation directly
  actual <- run $ do
    let xs' = VS.fromList xs
    actual' <- VS.thaw $ VS.fromList ys
    BI.axpyM n da xs' incx actual' incy
    VS.freeze actual'

  -- invariant: same size as ys always
  assert (length ys == VS.length actual)
  assert (length ys == VS.length expected)

  -- invariant: both methods give same answer
  let ass = VS.toList actual
      ess = VS.toList expected
  assert $ and $ zipWith (\a e -> a ~== coerce e) ass ess


-- --
prop_scalM :: BlasArgs Double -> Double -> Property
prop_scalM (BlasArgs (Positive n) xs (NonZero incx) _ _) da = monadicIO $ do
  -- expected uses the CBLAS implementation via inline-c
  expected <- run $ do
    let expected' = VS.fromList (coerce xs)
        da' = coerce da
        n' = fromIntegral n
        incx' = fromIntegral incx
    cblas_scalW n' da' expected' incx' 
    return expected'
    
  -- actual calls the monadic haskell implementation directly
  actual <- run $ do
    actual' <- VS.thaw $ VS.fromList xs
    BI.scalM n da actual' incx
    VS.freeze actual'

  -- invariant: same size as ys always
  assert (length xs == VS.length actual)
  assert (length xs == VS.length expected)

  -- invariant: both methods give same answer
  let ass = VS.toList actual
      ess = VS.toList expected
  assert $ and $ zipWith (\a e -> a ~== coerce e) ass ess
  

-- --
prop_swapM :: BlasArgs Double -> Property
prop_swapM (BlasArgs (Positive n) xs (NonZero incx) ys (NonZero incy)) = monadicIO $ do
  -- expected uses the CBLAS implementation via inline-c
  (expectedXS, expectedYS) <- run $ do
    let
      expectedXS' = VS.fromList (coerce xs)
      expectedYS' = VS.fromList (coerce ys)
      n' = fromIntegral n
      incx' = fromIntegral incx
      incy' = fromIntegral incy
    cblas_swapW n' expectedXS' incx' expectedYS' incy'
    return (expectedXS', expectedYS')
    
  -- actual calls the monadic haskell implementation directly
  (actualXS, actualYS) <- run $ do
    actualXS' <- VS.thaw $ VS.fromList xs
    actualYS' <- VS.thaw $ VS.fromList ys
    BI.swapM n actualXS' incx actualYS' incy
    actualXS'' <- VS.freeze actualXS'
    actualYS'' <- VS.freeze actualYS'
    return (actualXS'', actualYS'')
    
  -- invariant: same size as ys always
  assert (length xs == VS.length actualXS)
  assert (length xs == VS.length expectedXS)
  assert (length ys == VS.length actualYS)
  assert (length ys == VS.length expectedYS)

  -- invariant: both methods give same answer
  assert $ and $ zipWith (\a e -> a ~== coerce e) (VS.toList actualXS) (VS.toList expectedXS)
  assert $ and $ zipWith (\a e -> a ~== coerce e) (VS.toList actualYS) (VS.toList expectedYS)
  

