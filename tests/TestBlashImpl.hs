{-# LANGUAGE DeriveGeneric #-}

module TestBlashImpl where

import           Data.Coerce (coerce)
import qualified Data.Vector.Storable as VS
import           Foreign.C.Types
import           Test.QuickCheck (vector, genericShrink, Arbitrary(..), Positive(..), NonZero(..))
import           Test.QuickCheck.Property (Property)
import           Test.QuickCheck.Monadic
import           GHC.Generics (Generic)
import           Data.AEq 
import qualified BlashImpl as BI  
import           OpenBLAS1  
  

data TArgs = TArgs (Positive Int) Int
            deriving (Eq, Show, Generic)

instance Arbitrary TArgs where
  arbitrary = do
    n'@(Positive n) <- arbitrary
    return $ TArgs n' (n+1)
    
  shrink = genericShrink


prop_readAndSum :: TArgs -> Property
prop_readAndSum (TArgs (Positive n) _) = monadicIO $ do
  x <- run $ readAndSumW (n+1)
  assert $ x ~== (sum [1..n])


-- need special arbitrary instance to make the inc, n, vectors sizes work out  
data BlasArgs a = BlasArgs (Positive BI.Size) [a] (NonZero BI.Inc) [a] (NonZero BI.Inc)
                deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (BlasArgs a) where
  arbitrary = do
    n@(Positive n') <- arbitrary
    incx@(NonZero incx') <- arbitrary
    incy@(NonZero incy') <- arbitrary
    xs <- vector (n'*(abs incx'))
    ys <- vector (n'*(abs incy'))
    return $ BlasArgs n xs incx ys incy
    
  shrink (BlasArgs n xs incx ys incy) = do
    n''@(Positive n') <- shrink n
    incx''@(NonZero incx') <- shrink incx
    incy''@(NonZero incy') <- shrink incy
    -- relying on fact that shrink for n and inc
    -- means get smaller number
    let xs'' = take (n'*(abs incx')) xs
        ys'' = take (n'*(abs incy')) ys
    return (BlasArgs n'' xs'' incx'' ys'' incy'')


prop_blasargs :: BlasArgs Double -> Property
prop_blasargs (BlasArgs (Positive n) xs (NonZero incx) ys (NonZero incy)) = monadicIO $ do
  assert $ (n*(abs incx)) == length xs
  assert $ (n*(abs incy)) == length ys

  
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
  

