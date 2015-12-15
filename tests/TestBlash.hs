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
  Hspec.describe "dcopy haskell implementation" $ do
    Hspec.it "calling readAndSum: helping understand monadic properties" $ property $ prop_readAndSum
    Hspec.it "dcopyM should compare exactly to inline-c cblas_dcopyW" $ property $ prop_dcopyM
    
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
data CopyArgs a = CopyArgs {
    n_ :: Int
  , xs_ :: [a]
  , incx_ :: Int
  , ys_ :: [a]
  , incy_ :: Int
  } deriving (Eq, Show, Generic)

instance (Arbitrary a) => Arbitrary (CopyArgs a) where
  arbitrary = do
    Positive n <- arbitrary
    Positive incx <- arbitrary
    Positive incy <- arbitrary
    xs <- vector (n*incx)
    ys <- vector (n*incy)
    return $ CopyArgs n xs incx ys incy
    
  shrink = genericShrink

prop_dcopyM :: CopyArgs Double -> Property
prop_dcopyM (CopyArgs n xs incx ys incy) = monadicIO $ do
  -- expected uses the CBLAS implementation via inline-c
  expected <- run $ do
    let expected' = VS.fromList (coerce ys)
        xs' = VS.fromList (coerce xs)
    cblas_dcopyW n xs' incx expected' incy
    return expected'
    
  -- actual calls the monadic haskell implementation directly
  actual <- run $ do
    let xs' = VS.fromList xs
    actual' <- VS.thaw $ VS.fromList ys
    M.dcopyM n xs' incx actual' incy
    VS.freeze actual'

  -- invariant: same size as ys always
  assert (length ys == VS.length actual)
  assert (length ys == VS.length expected)

  -- invariant: both methods give same answer
  let ass = VS.toList actual
      ess = VS.toList expected
  assert $ and $ zipWith (\a e -> a == coerce e) ass ess
  
cblas_dcopyW :: Int -> VS.Vector CDouble -> Int -> VS.Vector CDouble -> Int -> IO ()
cblas_dcopyW n _  incx _  incy | n <= 0 || incx <= 0 || incy <= 0 = return ()
cblas_dcopyW n dx incx dy incy = do
  let n' = fromIntegral n
      incx' = fromIntegral incx
      incy' = fromIntegral incy
  [C.block| void
   {
     cblas_dcopy(
         $(int n'),
         $vec-ptr:(double* dx),
         $(int incx'),
         $vec-ptr:(double* dy),
         $(int incy')
         );
   }
   |]
  
     -- int n_ = $(int n');
     -- dcopy___(
     --     &n_,
     --     $vec-ptr:(const double* dx),
     --     $(int incx'),
     --     $vec-ptr:(double* dy),
     --     $(int incy')
     --     );

