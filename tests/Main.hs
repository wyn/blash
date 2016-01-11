module Main where

import Test.QuickCheck.Property (property)
import qualified Test.Hspec as Hspec
import TestBlash
import TestBlashImpl
import TestBlashData


main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "BLAS haskell implementation - comparing to openblas functions" $ do
    Hspec.it "calling readAndSum: helping understand monadic quickcheck" $ property $ prop_readAndSum
    Hspec.it "BlasArgs should have invarient that vector length == n*inc for x & y" $ property $ prop_blasargs
    Hspec.it "copyM should compare to dcopy" $ property $ prop_copyM
    Hspec.it "axpyM should compare to daxpy" $ property $ prop_axpyM
    Hspec.it "scalM should compare to scal" $ property $ prop_scalM
    Hspec.it "swapM should compare to swap" $ property $ prop_swapM
    Hspec.it "dot product should compare to ddot" $ property $ prop_dot
    Hspec.it "nrm2 should compare to dnrm2" $ property $ prop_nrm2
    Hspec.it "asum should compare to dasum" $ property $ prop_asum
    Hspec.it "iamax should compare to damax" $ property $ prop_iamax
    Hspec.it "rotg should compare to drotg" $ property $ prop_rotg 
