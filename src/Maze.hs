module Maze
  ( -- * Maze type
    IMaze
  , iMazeBounds
  , iMazeMove
  , iMazeCoords
  , iMazeGetCell
  , iMazeEntranceCoord
  , sample
  , iBlankCoords
  , Coord
  , coordRow
  , coordCol
  , getRow
  , getCol
  , newCoord
  , Cell
  , isWall
  , Direction(..)
    -- * Maze construction algorithms
  , randomGeneration
  ) where

import Maze.Core
import Maze.Algorithms
