module Main where

import Brick
import qualified Brick as B
import Brick.BChan
import Control.Concurrent
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text
import Data.Time.Clock
import Graphics.Vty
import Graphics.Vty.Platform.Unix (mkVty)
import Maze.Core (Direction (..))
import Network.WebSockets
import System.Process (system)
import System.Random
import Text.Parsec
import Text.Parsec.ByteString (Parser)
import UI

type SharedGameState = MVar GameState

-- mazeGen :: IO ()
-- mazeGen = do
--   let builder = mkVty defaultConfig
--   initialVty <- builder
--   eventChannel <- newBChan 10
--   void . forkIO $ forever $ do
--     t <- getCurrentTime
--     writeBChan eventChannel (Tick t)
--     threadDelay 100000

--   st <- getCurrentTime
--   g <- getStdGen
--   let gs = (gameState g 10 10 RecursiveBacktracking Big st st)
--   void $
--     customMain
--       initialVty
--       builder
--       (Just eventChannel)
--       mazeApp
--       gs
--   -- return gs

mazeGen :: BChan MazeEvent -> SharedGameState -> IO ()
mazeGen eventChannel sharedState = do
  let builder = mkVty defaultConfig
  initialVty <- builder
  -- eventChannel <- newBChan 10
  void . forkIO $ forever $ do
    t <- getCurrentTime
    writeBChan eventChannel (Tick t)
    threadDelay 100000

  st <- getCurrentTime
  g <- getStdGen
  let initialGameState = gameState g 10 10 RecursiveBacktracking Big st st
  putMVar sharedState initialGameState

  void $ customMain initialVty builder (Just eventChannel) mazeApp initialGameState

data Command = Move Maze.Core.Direction | Quit deriving (Show, Eq)

directionP :: Parser Maze.Core.Direction
directionP =
  (Text.Parsec.char 'w' >> return DUp)
    Text.Parsec.<|> (Text.Parsec.char 's' >> return DDown)
    Text.Parsec.<|> (Text.Parsec.char 'a' >> return DLeft)
    Text.Parsec.<|> (Text.Parsec.char 'd' >> return DRight)

commandP :: Parser Command
commandP = (Move <$> directionP) Text.Parsec.<|> (Text.Parsec.char 'q' >> return Quit)

parseCommand :: ByteString -> Either ParseError Command
parseCommand = runParser commandP () ""

type ServerState = [(String, Connection)]

-- server :: ServerApp
-- server pending = do
--   conn <- acceptRequest pending
--   eventChannel <- newBChan 1000

--   withPingThread conn 30 (return ()) $
--     forever $ do
--       msg <- receiveData conn
--       putStr $ "Received: " ++ unpack (msg :: ByteString)
--       case parseCommand msg of
--         Left err -> putStrLn " (invalid)"
--         Right Quit -> putStrLn " (quit)"
--         -- handleEvent (VtyEvent (EvKey KUp []))
--         Right (Move DUp) -> do
--           writeBChan eventChannel (ClientMove DUp)
--           putStrLn " (up)"
--         Right (Move DDown) -> do
--           writeBChan eventChannel (ClientMove DDown)
--           putStrLn " (down)"
--         Right (Move DLeft) -> do
--           writeBChan eventChannel (ClientMove DLeft)
--           putStrLn " (left)"
--         Right (Move DRight) -> do
--           writeBChan eventChannel (ClientMove DRight)
--           putStrLn " (right)"
--       return ()

server :: BChan MazeEvent -> SharedGameState -> ServerApp
server eventChannel sharedState pending = do
  conn <- acceptRequest pending
  -- eventChannel <- newBChan 10

  withPingThread conn 30 (return ()) $ forever $ do
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
  where i = 1 -- TODO: assign player id from server state

-- main :: IO ()
-- main = do
--   -- run maze and server in parallel
--   let gs = Nothing

--   putStrLn "Generating maze..."
--   _ <- async mazeGen

--   putStrLn "Starting server..."
--   serverTask <- async $ runServer "localhost" 9160 server

--   wait serverTask

main :: IO ()
main = do
  sharedState <- newEmptyMVar
  eventChannel <- newBChan 10
  putStrLn "Generating maze..."
  _ <- async $ mazeGen eventChannel sharedState
  putStrLn "Starting server..."
  serverTask <- async $ runServer "localhost" 9160 (server eventChannel sharedState)
  wait serverTask
