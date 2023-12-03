module Main where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Maze.Core (Direction (..))
import Network.WebSockets
import System.Process (system)
import Text.Parsec
import Text.Parsec.ByteString (Parser)

data Command = Move Direction | Quit deriving (Show, Eq)

directionP :: Parser Direction
directionP =
  (char 'w' >> return DUp)
    <|> (char 'a' >> return DDown)
    <|> (char 's' >> return DLeft)
    <|> (char 'd' >> return DRight)

commandP :: Parser Command
commandP = (Move <$> directionP) <|> (char 'Q' >> return Quit)

parseCommand :: ByteString -> Either ParseError Command
parseCommand = runParser commandP () ""

type ServerState = [(String, Connection)]

server :: ServerApp
server pending = do
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $
    forever $ do
      msg <- receiveData conn
      putStr $ "Received: " ++ unpack (msg :: ByteString)
      case parseCommand msg of
        Left err -> putStrLn " (invalid)"
        Right Quit -> putStrLn " (quit)"
        Right (Move DUp) -> putStrLn " (up)"
        Right (Move DDown) -> putStrLn " (down)"
        Right (Move DLeft) -> putStrLn " (left)"
        Right (Move DRight) -> putStrLn " (right)"
      return ()

main :: IO ()
main = do
  system "clear"
  runServer "0.0.0.0" 9160 server
