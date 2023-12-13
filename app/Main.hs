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
  forkIO $ forever $ getCurrentTime >>= writeBChan eventChannel . Tick
    >> threadDelay 100000  -- tick every 0.1 seconds
  forkIO $ forever $ writeBChan eventChannel MonsterTick
    >> threadDelay 1000000  -- move monster every second

  st <- getCurrentTime
  g <- getStdGen
  void $ customMain
    initialVty
    builder
    (Just eventChannel)
    mazeApp (initGameState maxRows maxCols 1 BinaryTree Big g st st)
