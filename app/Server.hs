module Main where

import Brick
import Brick.BChan
import Control.Concurrent
import Control.Concurrent.Async (async, wait)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Time.Clock
import Graphics.Vty (defaultConfig)
import Graphics.Vty.Platform.Unix (mkVty)
import Maze.Core (Direction (..))
import Network.WebSockets
import System.Process (system)
import System.Random
import Text.Parsec
import Text.Parsec.ByteString (Parser)
import UI

mazeGen :: BChan MazeEvent -> IO ()
mazeGen eventChannel = do
  let builder = mkVty defaultConfig
  initialVty <- builder
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
    mazeApp (initGameState 10 10 4 RecursiveBacktracking Big g st st)

data Command = Move Maze.Core.Direction | Quit deriving (Show, Eq)

directionP :: Parser Maze.Core.Direction
directionP =
  (char 'w' >> return DUp)
    <|> (char 's' >> return DDown)
    <|> (char 'a' >> return DLeft)
    <|> (char 'd' >> return DRight)

commandP :: Parser Command
commandP = (Move <$> directionP) <|> (char 'q' >> return Quit)

parseCommand :: ByteString -> Either ParseError Command
parseCommand = runParser commandP () ""

server :: BChan MazeEvent -> ServerApp
server eventChannel pending = do
  conn <- acceptRequest pending
  -- eventChannel <- newBChan 10
  counter <- newMVar 1

  withPingThread conn 30 (return ()) $ do
    i <- takeMVar counter
    putMVar counter (i + 1)
    forever $ do
      msg <- receiveData conn
      -- putStrLn $ "Received: " ++ Data.ByteString.Char8.unpack msg
      case parseCommand msg of
        Left err -> putStrLn " (invalid command)"
        Right Quit -> do
          putStrLn " (quit)"
          writeBChan eventChannel QuitGame
        -- sendClose conn (Data.ByteString.Char8.pack "Quit")
        Right (Move dir) -> do
          -- putStrLn $ "Client sent move command: " ++ show dir
          writeBChan eventChannel (ClientMove i dir)

main :: IO ()
main = do
  eventChannel <- newBChan 10
  putStrLn "Generating maze..."
  async $ mazeGen eventChannel
  putStrLn "Starting server..."
  serverTask <- async $ runServer "localhost" 9160 (server eventChannel)
  wait serverTask