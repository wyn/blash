module TestBlash where

import           Data.Coerce (coerce)
import qualified Data.Vector.Storable as VS
import           Foreign.C.Types
import           Test.QuickCheck (Positive(..), NonZero(..))
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property (Property)
import           Data.AEq 
import qualified Blash as B
import           OpenBLAS1
import           TestBlashData


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


