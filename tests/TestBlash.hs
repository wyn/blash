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
import           Test.QuickCheck (vector, genericShrink, Arbitrary(..), Positive(..), NonZero(..))
import           Test.QuickCheck.Property (property, Property)
import           Test.QuickCheck.Monadic
import           GHC.Generics (Generic)
import           Data.AEq 
import qualified Blash as B
import qualified BlashImpl as BI


C.context (C.baseCtx <> C.vecCtx)

C.include "<math.h>"
C.include "<string.h>"
C.include "atest.h"
C.include "cblas.h"

    
main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "BLAS haskell implementation" $ do
    Hspec.it "calling readAndSum: helping understand monadic quickcheck" $ property $ prop_readAndSum
    Hspec.it "BlasArgs should have invarient that vector length == n*inc for x & y" $ property $ prop_blasargs
    Hspec.it "copyM should compare to inline-c cblas_dcopyW" $ property $ prop_copyM
    Hspec.it "axpyM should compare to inline-c cblas_daxpyW" $ property $ prop_axpyM
    Hspec.it "dot product should compare to inline-c cblas_ddotW" $ property $ prop_dot
    Hspec.it "nrm2 should compare to inline-c cblas_dnrm2W" $ property $ prop_nrm2
    Hspec.it "asum should compare to inline-c cblas_dasumW" $ property $ prop_asum
    Hspec.it "iamax should compare to inline-c cblas_damaxW" $ property $ prop_iamax
    Hspec.it "scalM should compare to inline-c cblas_scalW" $ property $ prop_scalM
  
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
    BI.axpyM n da xs' incx actual' incy
    VS.freeze actual'

  -- invariant: same size as ys always
  assert (length ys == VS.length actual)
  assert (length ys == VS.length expected)

  -- invariant: both methods give same answer
  let ass = VS.toList actual
      ess = VS.toList expected
  assert $ and $ zipWith (\a e -> a ~== coerce e) ass ess


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
  assert $ actual ~== coerce expected
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


prop_nrm2 :: BlasArgs Double -> Property
prop_nrm2 (BlasArgs (Positive n) xs (NonZero incx) _ _) = monadicIO $ do
  expected <- run $ do
    let xs' = VS.fromList (coerce xs)
        n' = fromIntegral n
        incx' = fromIntegral incx
    cblas_nrm2W n' xs' incx'

  -- invariant: both methods give same answer
  assert $ actual ~== coerce expected
    where 
      -- actual calls the haskell implementation directly
      actual = B.nrm2 n (VS.fromList xs) incx


cblas_nrm2W :: CInt -> VS.Vector CDouble -> CInt -> IO (CDouble)
cblas_nrm2W n dx incx = do
  [C.block| double
   {
     return cblas_dnrm2(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx)
         );
   }
   |]



-- --
prop_asum :: BlasArgs Double -> Property
prop_asum (BlasArgs (Positive n) xs (NonZero incx) _ _) = monadicIO $ do
  expected <- run $ do
    let xs' = VS.fromList (coerce xs)
        n' = fromIntegral n
        incx' = fromIntegral incx
    cblas_asumW n' xs' incx'

  -- run $ putStrLn $ "n: " ++ show n
  -- run $ putStrLn $ "xs:   " ++ show xs
  -- run $ putStrLn $ "incx: " ++ show incx
  -- run $ putStrLn $ "expected:   " ++ show expected
  -- run $ putStrLn $ "actual:   " ++ show actual
  -- -- invariant: both methods give same answer
  assert $ actual ~== coerce expected
    where 
      -- actual calls the haskell implementation directly
      actual = B.asum n (VS.fromList xs) incx


cblas_asumW :: CInt -> VS.Vector CDouble -> CInt -> IO (CDouble)
cblas_asumW n dx incx = do
  [C.block| double
   {
     return cblas_dasum(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx)
         );
   }
   |]


-- --
prop_iamax :: BlasArgs Double -> Property
prop_iamax (BlasArgs (Positive n) xs (NonZero incx) _ _) = monadicIO $ do
  expected <- run $ do
    let xs' = VS.fromList (coerce xs)
        n' = fromIntegral n
        incx' = fromIntegral incx
    cblas_iamaxW n' xs' incx'

  -- -- invariant: both methods give same answer
  assert $ actual == expected
    where 
      -- actual calls the haskell implementation directly
      actual = fromIntegral $ B.iamax n (VS.fromList xs) incx


cblas_iamaxW :: CInt -> VS.Vector CDouble -> CInt -> IO (CInt)
cblas_iamaxW n dx incx = do
  [C.block| int
   {
     return cblas_idamax(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx)
         );
   }
   |]



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
  

cblas_scalW :: CInt -> CDouble -> VS.Vector CDouble -> CInt -> IO ()
cblas_scalW n da dx incx = do
  [C.block| void
   {
     cblas_dscal(
         $(const int n),
         $(const double da),
         $vec-ptr:(double* dx),
         $(const int incx)
         );
   }
   |]

  
-- --
