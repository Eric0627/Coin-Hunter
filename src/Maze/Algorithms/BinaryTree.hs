module Maze.Algorithms.BinaryTree
  ( binaryTree,
  prop_mazeHasPath_binaryTree,
  ) where

import Maze.Core

import Control.Monad ( forM_, when, void )
import Control.Monad.ST
import Data.Ix
import Data.STRef
import Data.Word
import System.Random
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Test.QuickCheck

-- | Build a maze using the "binary tree" algorithm. For each cell, randomly
-- remove the wall above or to the left, but not both.
binaryTree :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
binaryTree g rows cols = runST $ do
  gRef <- newSTRef g
  maze <- newSTMaze rows cols
  coords <- range <$> stMazeBounds maze
  forM_ coords $ \pos -> do
    g <- readSTRef gRef
    let (b, g') = random g
    if b
      then do success <- stMazeOpenCoordDir maze pos DUp
              when (not success) $ void $ stMazeOpenCoordDir maze pos DLeft
      else do success <- stMazeOpenCoordDir maze pos DLeft
              when (not success) $ void $ stMazeOpenCoordDir maze pos DUp
    writeSTRef gRef g'
  imaze <- freezeSTMaze maze
  g' <- readSTRef gRef
  return (imaze, g')

dfs :: IMaze -> Coord -> Coord -> Set.Set Coord -> Bool
dfs maze currentCoord targetCoord visited
  | currentCoord == targetCoord = True
  | currentCoord `Set.member` visited = False
  | otherwise =
      let newVisited = Set.insert currentCoord visited
          neighbors = filter (`Set.notMember` visited) $ adjacentCoords maze currentCoord
      in any (\nextCoord -> dfs maze nextCoord targetCoord newVisited) neighbors

-- Function to get adjacent coordinates in the maze
adjacentCoords :: IMaze -> Coord -> [Coord]
adjacentCoords maze coord =
      let directions = [DUp, DDown, DLeft, DRight] -- All possible directions
          moveResults = map (\dir -> iMazeMove maze coord dir) directions
      in catMaybes moveResults

start :: Coord
start = C 0 0
end :: Coord
end = C 9 9

hasPath :: IMaze -> Bool
hasPath maze = dfs maze start end Set.empty

-- >>> quickCheck prop_mazeHasPath_binaryTree
-- +++ OK, passed 100 tests.
--

-- Property that asserts every generated maze has a path
prop_mazeHasPath_binaryTree :: Int -> Bool
prop_mazeHasPath_binaryTree seed =
  let (maze, _) = binaryTree (mkStdGen seed) 10 10
  in hasPath maze