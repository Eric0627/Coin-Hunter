{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module stores the basic types and low-level operations for building
-- 2-dimensional rectangular mazes. We build the maze in 'ST' and then freeze it
-- when we're done, using mutable and immutable arrays.
module Maze.Core
  ( -- * Coordinates, directions, and walls
    Coord,
    coordRow,
    coordCol,
    getRow,
    getCol,
    newCoord,
    Cell,
    isWall,
    Direction (..),
    -- * Mutable maze
    STMaze,
    newSTMaze,
    stMazeBounds,
    stMazeNeighborCoords,
    stMazeCoordBlocked,
    -- * Immutable maze
    IMaze,
    freezeSTMaze,
    iMazeBounds,
    iMazeMove,
    iMazeCoords,
    iMazeGetCell,
    iMazeEntranceCoord,
    sample,
    iBlankCoords,
  )
where

import Control.Monad (filterM, when)
import Control.Monad.Extra (allM)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Bits
import Data.Word
import System.Random

maxRows :: Word32
maxRows = 30

maxCols :: Word32
maxCols = 50

-- | A single cell of a 2-dimensional maze.
newtype Cell = Cell
  { -- | Is this cell a wall?
    isWall :: Bool
  }
  deriving (Show)

-- | Create a fresh cell with both right and down closed.
newCell :: Cell
newCell = Cell False

-- | The location of a cell within a maze.
data Coord = C
  { -- | @0@-indexed row of coordinate
    coordRow :: Word32,
    -- | @0@-indexed column of coordinate
    coordCol :: Word32
  }
  deriving (Show, Eq, Ord, Ix)

-- | Get row from a coord.
getRow :: Coord -> Word32
getRow (C r _) = r

-- | Get column from a coord.
getCol :: Coord -> Word32
getCol (C _ c) = c

-- | Generate a new coord from row and colomn.
newCoord :: Word32 -> Word32 -> Coord
newCoord = C

-- | Represents a direction relating one cell to another.
data Direction = DUp | DDown | DLeft | DRight deriving (Show, Eq)

-- | Get the opposite direction.
flipDirection :: Direction -> Direction
flipDirection DUp = DDown
flipDirection DDown = DUp
flipDirection DLeft = DRight
flipDirection DRight = DLeft

-- | Get the neighbor of a cell in a particular direction. Since we don't check
-- bounds, this can return a coordinate outside the maze (including a negative
-- coordinate), so always use in conjunction with 'stMazeInBounds'.
neighborCoord :: Direction -> Coord -> Coord
neighborCoord dir (C r c) = case dir of
  DUp -> C (r - 1) c
  DDown -> C (r + 1) c
  DLeft -> C r (c - 1)
  DRight -> C r (c + 1)

-- | Mutable maze in 'ST' monad.
newtype STMaze s = STMaze {stMazeArray :: STArray s Coord Cell}

-- | Construct a new 'STMaze' with a given number of rows and columns. Both rows
-- and columns must be positive, or this function will throw an error.
newSTMaze :: Word32 -> Word32 -> ST s (STMaze s)
newSTMaze 0 _ = error "newSTMaze called with 0 rows"
newSTMaze _ 0 = error "newSTMaze called with 0 columns"
newSTMaze rows cols = STMaze <$> newArray (C 0 0, C (rows - 1) (cols - 1)) newCell

-- | Get the bounds of an 'STMaze' (top-left and bottom-right corners).
stMazeBounds :: STMaze s -> ST s (Coord, Coord)
stMazeBounds = getBounds . stMazeArray

-- | Determine if a coordinate lies within an 'STMaze'\'s bounds.
stMazeInBounds :: STMaze s -> Coord -> ST s Bool
stMazeInBounds maze pos = do
  bounds <- stMazeBounds maze
  return $ inRange bounds pos

-- | Get the neighbor coordinate in a particular direction of an 'STMaze', if
-- one exists. If it doesn't, return 'Nothing'.
stMazeNeighborCoord :: STMaze s -> Direction -> Coord -> ST s (Maybe Coord)
stMazeNeighborCoord maze dir pos = do
  let nPos = neighborCoord dir pos
  inB <- stMazeInBounds maze (neighborCoord dir pos)
  if inB then return (Just nPos) else return Nothing

-- | Get all the neighbors of a particular cell in an 'STMaze', along with their
-- directions relative to the input cell. It doesn't matter whether there is a
-- wall between the cells.
stMazeNeighborCoords :: STMaze s -> Coord -> ST s [(Direction, Coord)]
stMazeNeighborCoords maze pos =
  let ns = [(dir, neighborCoord dir pos) | dir <- [DUp, DDown, DLeft, DRight]]
   in filterM (stMazeInBounds maze . snd) ns

-- | Set the cell as a wall, given the coordinate of the cell. If the coordinate is
--  outside the maze, do nothing, but return 'False'.
stMazeCoordBlocked :: STMaze s -> Coord -> ST s Bool
stMazeCoordBlocked maze pos = do
  inBounds <- stMazeInBounds maze pos
  when inBounds $ do
    let arr = stMazeArray maze
    cell <- readArray arr pos
    writeArray arr pos (cell {isWall = True})
  return inBounds

-- | Immutable maze.
newtype IMaze = IMaze {iMazeArray :: Array Coord Cell}

-- | Freeze a mutable 'STMaze' to an immutable 'IMaze'.
freezeSTMaze :: STMaze s -> ST s IMaze
freezeSTMaze (STMaze a) = IMaze <$> freeze a

-- | Get the bounds of an 'IMaze' (top-left and bottom-right corners).
iMazeBounds :: IMaze -> (Coord, Coord)
iMazeBounds = bounds . iMazeArray

-- | Determine if a coordinate lies within an 'IMaze'\'s bounds.
iMazeInBounds :: IMaze -> Coord -> Bool
iMazeInBounds = inRange . bounds . iMazeArray

-- | Get the cell at a given coordinate of an 'IMaze'. Does not do bounds
-- checking, so this can raise an error.
iMazeGetCell :: IMaze -> Coord -> Cell
iMazeGetCell maze pos = iMazeArray maze ! pos

-- | Get the coordinate of the entrance.
iMazeEntranceCoord :: IMaze -> Coord
iMazeEntranceCoord maze = C (maxRows `div` 2) 0

-- | Get the coordinate of the exit.
iMazeExitCoord :: IMaze -> Coord
iMazeExitCoord maze = C (maxRows `div` 2) (maxCols - 1)

-- | Given a maze, a coordinate, and a direction we'd like to move, return the
-- coordinate we are trying to move to, if it is possible to do so; otherwise
-- return 'Nothing'.
iMazeMove :: IMaze -> Coord -> Direction -> Maybe Coord
iMazeMove maze pos dir =
  if not (isWall nCell) && iMazeInBounds maze nPos
    then Just nPos
    else Nothing
  where
    cell = iMazeGetCell maze pos
    nPos = neighborCoord dir pos
    nCell = iMazeGetCell maze nPos

-- | Extract a list of lists of coordinates from an 'IMaze' (including the border walls), in row-major order.
iMazeCoords :: IMaze -> [[Coord]]
iMazeCoords maze = rows
  where
    (_, C hiR hiC) = iMazeBounds maze
    rows =
      [ [ C (fromInteger r) (fromInteger c)
          | c <- [0 .. toInteger hiC]
        ]
        | r <- [0 .. toInteger hiR]
      ]

-- Function to sample k elements from a list
-- >>> sample (mkStdGen 42) 3 [(C 1 2), (C 2 2), (C 3 2), (C 4 2), (C 5 2)]
-- [C {coordRow = 4, coordCol = 2},C {coordRow = 2, coordCol = 2},C {coordRow = 2, coordCol = 2}]
--
sample :: (RandomGen g) => g -> Int -> [a] -> ([a], g)
sample gen 0 _ = ([], gen)
sample gen k xs =
  let (index, g') = randomR (0, length xs - 1) gen
      (xs', g'') = sample g' (k - 1) xs
   in (xs !! index : xs', g'')

-- | Get the cells' coordinates which are not walls.
iBlankCoords :: IMaze -> [Coord]
iBlankCoords maze = coords
  where
    (_, C hiR hiC) = iMazeBounds maze
    coords =
      [ C (fromInteger r) (fromInteger c)
        | c <- [1 .. toInteger hiC - 1],
          r <- [1 .. toInteger hiR - 1],
          not (isWall (iMazeGetCell maze (C (fromInteger r) (fromInteger c))))
      ]