{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

import           Data.Coerce (coerce)
import qualified Language.C.Inline as C
import qualified Data.Vector.Storable as VS
import           Foreign.C.Types
import           Data.Monoid ((<>))
import qualified Test.Hspec as Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import GHC.Generics (Generic)
import qualified BlashImpl as M


C.context (C.baseCtx <> C.vecCtx)

C.include "<math.h>"
C.include "<string.h>"
C.include "atest.h"
C.include "cblas.h"

    
main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "BLAS haskell implementation" $ do
    Hspec.it "calling readAndSum: helping understand monadic properties" $ property $ prop_readAndSum
    Hspec.it "copyM should compare exactly to inline-c cblas_dcopyW" $ property $ prop_copyM
    Hspec.it "axpyM should compare exactly to inline-c cblas_daxpyW" $ property $ prop_axpyM
    
readAndSumW :: Int -> IO (Int)
readAndSumW n = do
  let n' = fromIntegral n
  x <- [C.exp| int { readAndSum( $(int n') ) } |]
  return $ fromIntegral x

prop_readAndSum :: Int -> Property
prop_readAndSum n = monadicIO $ do
  x <- run $ readAndSumW (n+1)
  assert (x == (sum [1..n]))

-- need special arbitrary instance
-- to make the inc, n, vectors sizes
-- work out
data BlasArgs a = BlasArgs Int [a] Int [a] Int
                deriving (Eq, Show, Generic)

instance (Arbitrary a) => Arbitrary (BlasArgs a) where
  arbitrary = do
    Positive n <- arbitrary
    Positive incx <- arbitrary
    Positive incy <- arbitrary
    xs <- vector (n*incx)
    ys <- vector (n*incy)
    return $ BlasArgs n xs incx ys incy
    
  shrink = genericShrink

prop_copyM :: BlasArgs Double -> Property
prop_copyM (BlasArgs n xs incx ys incy) = monadicIO $ do
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
    M.copyM n xs' incx actual' incy
    VS.freeze actual'

  -- invariant: same size as ys always
  assert (length ys == VS.length actual)
  assert (length ys == VS.length expected)

  -- invariant: both methods give same answer
  let ass = VS.toList actual
      ess = VS.toList expected
  assert $ and $ zipWith (\a e -> a == coerce e) ass ess
  
cblas_copyW :: CInt -> VS.Vector CDouble -> CInt -> VS.Vector CDouble -> CInt -> IO ()
cblas_copyW n dx incx dy incy = do
  [C.block| void
   {
     cblas_dcopy(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx),
         $vec-ptr:(double* dy),
         $(const int incy)
         );
   }
   |]
  
prop_axpyM :: BlasArgs Double -> Double -> Property
prop_axpyM (BlasArgs n xs incx ys incy) da = monadicIO $ do
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
    M.axpyM n da xs' incx actual' incy
    VS.freeze actual'

  -- invariant: same size as ys always
  assert (length ys == VS.length actual)
  assert (length ys == VS.length expected)

  -- invariant: both methods give same answer
  let ass = VS.toList actual
      ess = VS.toList expected
  assert $ and $ zipWith (\a e -> a == coerce e) ass ess

cblas_axpyW :: CInt -> CDouble -> VS.Vector CDouble -> CInt -> VS.Vector CDouble -> CInt -> IO ()
cblas_axpyW n da dx incx dy incy = do
  [C.block| void
   {
     cblas_daxpy(
         $(const int n),
         $(const double da),
         $vec-ptr:(const double* dx),
         $(const int incx),
         $vec-ptr:(double* dy),
         $(const int incy)
         );
   }
   |]
