module Maze.Algorithms.RandomGeneration
  ( randomGeneration
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

-- | Build a maze using the "random generation" algorithm. For each cell, randomly
-- set it as a wall with probability of 0.2.
randomGeneration :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
randomGeneration g rows cols = runST $ do
  gRef <- newSTRef g
  maze <- newSTMaze rows cols

  forM_ [0.. toInteger (cols-1)] $ \c -> do
    void $ stMazeCoordBlocked maze (newCoord 0 (fromIntegral c :: Word32))
    void $ stMazeCoordBlocked maze (newCoord (rows-1) (fromIntegral c :: Word32))

  forM_ [0.. toInteger (rows-1)] $ \r -> do
    when (r /= (toInteger rows `div` 2)) $ do
      void $ stMazeCoordBlocked maze (newCoord (fromIntegral r:: Word32) 0)
      void $ stMazeCoordBlocked maze (newCoord (fromIntegral r :: Word32) (cols-1))

  forM_ [2.. toInteger (cols-3)] $ \c -> do
    let wallIndexs = []
    --let wallIndexs = take ((fromIntegral (rows - 2) :: Int) `div` 2) . nub $ randomRs (1,fromIntegral (rows - 2) :: Int) g
    forM_ [1..toInteger rows-1] $ \r -> do
      when (length wallIndexs < 4) $ do
        g <- readSTRef gRef
        let (flag, g') = randomR (0, 4 :: Int) g
        when (flag == (0 :: Int)) $ do
          let wallIndexs = newCoord (fromInteger r) (fromInteger c) : wallIndexs
          void $ stMazeCoordBlocked maze (newCoord (fromIntegral r :: Word32) (fromIntegral c :: Word32))
        writeSTRef gRef g'

  imaze <- freezeSTMaze maze
  g' <- readSTRef gRef
  return (imaze, g')
