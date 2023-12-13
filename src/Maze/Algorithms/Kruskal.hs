module Maze.Algorithms.Kruskal
  ( kruskal
  ) where

import Maze.Core
import Maze.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (forM_, forM)
import Control.Monad.ST
import Data.Ix
import Data.STRef
import Data.Tuple
import Data.UnionFind.ST
import Data.Word
import System.Random
import Data.Maybe (catMaybes)
import Test.QuickCheck hiding (shuffle)

-- | Build a maze using Kruskal's algorithm.
kruskal :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
kruskal g numRows numCols = runST $ do
  maze <- newSTMaze numRows numCols
  walls <- stMazeInnerWalls maze
  let (walls', g') = shuffle walls g
  k <- newKruskal maze
  forM_ walls' $ \e -> do
    let (pos, pos') = wallNeighbors e
    sameSet <- kruskalSameSet k pos pos'
    case sameSet of
      True -> return ()
      False -> do
        stMazeOpenWall maze e
        kruskalUnion k pos pos'
  imaze <- freezeSTMaze maze
  return (imaze, g')

type Id = Word32

-- | Bookkeeping data structure for Kruskal's algorithm.
data K s = K { coordPoint :: Map.Map Coord (Point s Coord) }

-- | Create a fresh 'Kruskal' where each coord is in a singleton set with a
-- distinct id.
newKruskal :: STMaze s -> ST s (K s)
newKruskal maze = do
  mazeBounds <- stMazeBounds maze
  let coords = range mazeBounds
  pairs <- forM coords $ \pos -> do
    pt <- fresh pos
    return (pos, pt)
  return $ K (Map.fromList pairs)

kruskalSameSet :: K s -> Coord -> Coord -> ST s Bool
kruskalSameSet k pos pos' =
  (coordPoint k Map.! pos) `equivalent` (coordPoint k Map.! pos')

kruskalUnion :: K s -> Coord -> Coord -> ST s ()
kruskalUnion k pos pos' = (coordPoint k Map.! pos) `union` (coordPoint k Map.! pos')


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

-- >>> quickCheck prop_mazeHasPath_kruskal 
-- +++ OK, passed 100 tests.
--


-- Property that asserts every generated maze has a path
prop_mazeHasPath_kruskal :: Int -> Bool
prop_mazeHasPath_kruskal seed =
  let (maze, _) = kruskal (mkStdGen seed) 10 10
  in hasPath maze
