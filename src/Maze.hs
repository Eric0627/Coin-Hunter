module Maze
  ( -- * Maze type
    IMaze
  , iMazeBounds
  , iMazeMove
  , iMazeCoords
  , iMazeGetCell
  , iMazeEntranceCoord
  , sample
  , iCoinCoords
  , Coord
  , coordRow
  , coordCol
  , getRow
  , getCol
  , getCoord
  , Cell
  , isWall
  , Direction(..)
    -- * Maze construction algorithms
  , recursiveBacktracking
  , binaryTree
  , kruskal
  , myAlgorithm
  ) where

import Maze.Core
import Maze.Algorithms
import Maze.Algorithms.MyAlgorithm

