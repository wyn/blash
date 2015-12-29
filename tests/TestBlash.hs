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
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           GHC.Generics (Generic)

import qualified Blash as B
import qualified BlashImpl as M


C.context (C.baseCtx <> C.vecCtx)

C.include "<math.h>"
C.include "<string.h>"
C.include "atest.h"
C.include "cblas.h"

    
main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "BLAS haskell implementation" $ do
    Hspec.it "calling readAndSum: helping understand monadic quickcheck" $ property $ prop_readAndSum
    Hspec.it "copyM should compare exactly to inline-c cblas_dcopyW" $ property $ prop_copyM
    Hspec.it "axpyM should compare exactly to inline-c cblas_daxpyW" $ property $ prop_axpyM
    Hspec.it "dot product should compare exactly to inline-c cblas_ddotW" $ property $ prop_dot
    
readAndSumW :: Int -> IO (Int)
readAndSumW n = do
  let n' = fromIntegral n
  x <- [C.exp| int { readAndSum( $(int n') ) } |]
  return $ fromIntegral x

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
  assert (x == (sum [1..n]))

-- need special arbitrary instance to make the inc, n, vectors sizes work out  
data BlasArgs a = BlasArgs (Positive M.Size) [a] (NonZero M.Inc) [a] (NonZero M.Inc)
                deriving (Eq, Show, Generic)

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
    let xs'' = take (n'*(abs incx')) xs
        ys'' = take (n'*(abs incy')) ys
    return (BlasArgs n'' xs'' incx'' ys'' incy'')


prop_copyM :: BlasArgs Double -> Property
prop_copyM (BlasArgs (Positive n) xs (NonZero incx) ys (NonZero incy)) = monadicIO $ do
  assert $ ((n*incx) == length xs) || (((-n)*incx) == length xs)
  assert $ ((n*incy) == length ys) || (((-n)*incy) == length ys)
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
prop_dot :: BlasArgs Double -> Property
prop_dot (BlasArgs (Positive n) xs (NonZero incx) ys (NonZero incy)) = monadicIO $ do
  -- expected uses the CBLAS implementation via inline-c
  -- run $ putStrLn $ "n: " ++ show n
  -- run $ putStrLn $ "incx: " ++ show incx
  -- run $ putStrLn $ "incy: " ++ show incy
  -- run $ putStrLn $ "xs: " ++ show xs
  -- run $ putStrLn $ "ys: " ++ show ys
  -- run $ putStrLn $ "n*incx: " ++ show (n*incx)
  -- run $ putStrLn $ "(-n)*incx: " ++ show ((-n)*incx)
  expected <- run $ do
    let ys' = VS.fromList (coerce ys)
        xs' = VS.fromList (coerce xs)
        n' = fromIntegral n
        incx' = fromIntegral incx
        incy' = fromIntegral incy
    cblas_dotW n' xs' incx' ys' incy'
  -- run $ putStrLn $ "expected: " ++ show expected
  -- run $ putStrLn $ "actual:   " ++ show actual
    
  -- invariant: both methods give same answer
  assert $ abs (actual - (coerce expected)) <= 1.0e-6
    where 
      -- actual calls the haskell implementation directly
      actual = B.dot n (VS.fromList xs) incx (VS.fromList ys) incy

cblas_dotW :: CInt -> VS.Vector CDouble -> CInt -> VS.Vector CDouble -> CInt -> IO (CDouble)
cblas_dotW n dx incx dy incy = do
  [C.block| double
   {
     return cblas_ddot(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx),
         $vec-ptr:(const double* dy),
         $(const int incy)
         );
   }
   |]
