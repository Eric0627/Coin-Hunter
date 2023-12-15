module Server where

import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (MVar, ThreadId, forkIO, modifyMVar, modifyMVar_, newMVar, putMVar, takeMVar, withMVar)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Graphics.Vty (Input (eventChannel))
import Maze (Direction (..), MazeEvent (..))
import Network.WebSockets
import System.Process.Extra (system)
import Text.Parsec (ParseError, char, runParser, (<|>))
import Text.Parsec.ByteString (Parser)

data Command = Move Direction | Quit deriving (Show, Eq)

directionP :: Parser Direction
directionP = (char 'w' >> return DUp) <|> (char 's' >> return DDown) <|> (char 'a' >> return DLeft) <|> (char 'd' >> return DRight)

commandP :: Parser Command
commandP = (Move <$> directionP) <|> (char 'q' >> return Quit)

parseCommand :: ByteString -> Either ParseError Command
parseCommand = runParser commandP () ""

-- handles incoming connections and sends to the event channel
server :: BChan MazeEvent -> MVar Int -> PendingConnection -> IO ()
server eventChannel counter pending = do
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $ do
    -- use modifyMVar in case of race conditions
    i <- modifyMVar counter $ \i -> return (i + 1, i)
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

-- Forks a new thread that runs the server. Returns an counter MVar.
forkServer :: BChan MazeEvent -> IO (IO ())
forkServer eventChannel = do
  system "clear"
  counter <- newMVar 1
  forkIO $ runServer "0.0.0.0" 9160 (server eventChannel counter)
  let reseter = modifyMVar_ counter $ \_ -> return 1

  return reseter