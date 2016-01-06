{-# LANGUAGE DeriveGeneric #-}

module TestBlashData where

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

  
