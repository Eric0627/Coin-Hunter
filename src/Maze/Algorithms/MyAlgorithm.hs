module Maze.Algorithms.MyAlgorithm
  ( myAlgorithm
  ) where

import Maze.Core

import Control.Monad ( forM_, when, void )
import Control.Monad.ST
import Data.Ix
import Data.STRef
import Data.Word
import System.Random
import Data.List (nub)
import GHC.Real

-- | Build a maze using the "binary tree" algorithm. For each cell, randomly
-- remove the wall above or to the left, but not both.
myAlgorithm :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
myAlgorithm g rows cols = runST $ do
  gRef <- newSTRef g
  maze <- newSTMaze rows cols

  forM_ [2.. toInteger (cols-3)] $ \c -> do
    let wallIndexs = take ((fromIntegral (rows - 2) :: Int) `div` 2) . nub $ randomRs (0,fromIntegral (rows - 2) :: Int) g 
    forM_ [0..toInteger rows] $ \r -> do
      when ((fromIntegral r :: Int) `elem` wallIndexs) $ do
        void $ stMazeCoordBlocked maze (getCoord (fromIntegral c :: Word32) (fromIntegral r :: Word32))

  imaze <- freezeSTMaze maze
  g' <- readSTRef gRef
  return (imaze, g')

randomN :: (RandomGen g) => g -> Int -> [a] -> [a]
randomN g n xs =
  let indexs = take n . nub $ randomRs (0, n-1) g
  in map (xs !!) indexs