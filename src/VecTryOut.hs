module VecTryOut where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import System.Random (StdGen, getStdGen, randomR)


shuffleM :: (PrimMonad m, V.Unbox a)
            => StdGen
            -> Int -- count to shuffle
            -> M.MVector (PrimState m) a
            -> m ()
shuffleM _   i _ | i <= 1 = return ()
shuffleM gen i v = do
  M.swap v i' index
  shuffleM gen' i' v
    where
      (index, gen') = randomR (0, i') gen
      i' = i - 1

shuffle :: V.Unbox a
           => StdGen
           -> V.Vector a
           -> V.Vector a
shuffle gen vector = V.modify (shuffleM gen n) vector
  where
    n = V.length vector

main :: IO ()
main = do
  gen <- getStdGen
  print $ shuffle gen $ V.enumFromTo 1 (20 :: Int)
  
-- main :: IO ()
-- main = do
--   vector <- V.replicate 10 (0 :: Int)

--   replicateM_ (10^6) $ do
--     i <- randomRIO (0, 9)
--     oldCount <- V.read vector i
--     V.write vector i (oldCount + 1)

--   ivector <- freeze vector
--   print ivector
  
-- main :: IO ()
-- main = do
--   let v0 = V.replicate 10 (0 :: Int)

--       loop v 0 = return v
--       loop v rest = do
--         i <- randomRIO (0, 9)
--         let oldCount = v ! i
--             v' = v // [(i, oldCount + 1)]
--         loop v' (rest-1)
--   vector <- loop v0 (10^6)
--   print vector
  
-- main :: IO ()
-- main = do
--   let list = [1..10] :: [Int]
--       vector1 = V.fromList list :: V.Vector Int
--       vector2 = V.enumFromTo 1 10 :: V.Vector Int
--   print $ vector1 == vector2
--   print $ V.filter odd vector1
--   print $ V.dropWhile (<6) vector1
--   print $ V.foldr (*) 1 vector1


  
-- main :: IO ()
-- main = do
--   print $ V.sum $ V.enumFromTo 1 (10^9 :: Int)


