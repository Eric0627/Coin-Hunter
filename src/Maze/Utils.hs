module Maze.Utils
  ( shuffle
  ) where

import Math.Combinat.Permutations

import System.Random
import Test.QuickCheck hiding (shuffle)
import Data.List (sort)

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle [] g = ([], g)
shuffle as g =
  let len = length as
      (perm, g') = randomPermutation len g
  in (permuteList perm as, g')

-- >>> quickCheck prop_shuffleLength
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck prop_shuffleSameElements
-- +++ OK, passed 100 tests.
--

-- Custom generator for StdGen
stdGenGen :: Gen StdGen
stdGenGen = mkStdGen <$> arbitrary

-- Property 1: Shuffled list has the same length
prop_shuffleLength :: [Int] -> Property
prop_shuffleLength xs = 
    forAll stdGenGen $ \gen ->
        length (fst (shuffle xs gen)) == length xs

-- Property 2: Shuffled list contains the same elements
prop_shuffleSameElements :: [Int] -> Property
prop_shuffleSameElements xs = 
    forAll stdGenGen $ \gen ->
        let shuffled = fst (shuffle xs gen)
        in sort shuffled == sort xs