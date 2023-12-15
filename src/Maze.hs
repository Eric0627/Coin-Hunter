module Maze
  ( -- * Maze type
    IMaze,
    iMazeBounds,
    iMazeMove,
    iMazeCoords,
    sample,
    iCoinCoords,
    neighborCoord,
    Coord,
    coordRow,
    coordCol,
    Direction (..),
    MazeEvent (..),

    -- * Maze construction algorithms
    binaryTree,
  )
where

import Data.Time.Clock (UTCTime)
import Maze.Algorithms
import Maze.Core

data MazeEvent = QuitGame | ClientMove Int Direction | Tick UTCTime | MonsterTick
