module Main where

import Brick
import Brick.BChan
import Control.Concurrent
import Control.Monad (forever, void)
import Data.Time.Clock
import Graphics.Vty
import Graphics.Vty.Platform.Unix (mkVty)
import System.Random
import UI

main :: IO ()
main = do
  let builder = mkVty defaultConfig
  initialVty <- builder
  eventChannel <- newBChan 10
  void . forkIO $ forever $ do
    t <- getCurrentTime
    writeBChan eventChannel (TimeTick t)
    threadDelay 100000
  void . forkIO $ forever $ do
    t <- getCurrentTime
    writeBChan eventChannel (MonsterTick t) -- decide how fast monsters move
    threadDelay 1000000 -- Delay for 0.5 seconds

  st <- getCurrentTime
  g <- getStdGen
  void $
    customMain
      initialVty
      builder
      (Just eventChannel)
      mazeApp
      (gameState g 10 10 BinaryTree Big st st)
