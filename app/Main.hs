module Main where

import Brick.BChan ( newBChan )
import UI ( mazeGen )

main :: IO ()
main = newBChan 10 >>= mazeGen 1
