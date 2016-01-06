module TestMain where

import Test.QuickCheck.Property (property)
import qualified Test.Hspec as Hspec
import TestBlash
import TestBlashImpl


main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "BLAS haskell implementation - mutable stuff" $ do
    Hspec.it "calling readAndSum: helping understand monadic quickcheck" $ property $ prop_readAndSum
    Hspec.it "BlasArgs should have invarient that vector length == n*inc for x & y" $ property $ prop_blasargs
    Hspec.it "copyM should compare to inline-c cblas_dcopyW" $ property $ prop_copyM
    Hspec.it "axpyM should compare to inline-c cblas_daxpyW" $ property $ prop_axpyM
    Hspec.it "scalM should compare to inline-c cblas_scalW" $ property $ prop_scalM
    Hspec.it "swapM should compare to inline-c cblas_swapW" $ property $ prop_swapM
    Hspec.it "dot product should compare to inline-c cblas_ddotW" $ property $ prop_dot
    Hspec.it "nrm2 should compare to inline-c cblas_dnrm2W" $ property $ prop_nrm2
    Hspec.it "asum should compare to inline-c cblas_dasumW" $ property $ prop_asum
    Hspec.it "iamax should compare to inline-c cblas_damaxW" $ property $ prop_iamax
