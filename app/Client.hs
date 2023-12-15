{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Brick.AttrMap
import Brick.Focus
import Brick.Main
import Brick.Types
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import Graphics.Vty
import Network.WebSockets
import Prelude hiding (unlines)

data Name = Message deriving (Eq, Ord, Show)

data State = State {conn :: Connection, command :: Char}

updateCommand :: Char -> EventM n State ()
updateCommand c = do
  s@(State conn _) <- get
  put s {command = c}
  when (c `elem` ['w', 'a', 's', 'd', 'q']) (liftIO . sendTextData conn . pack $ [c])

showMessage :: String -> Widget n
showMessage = center . str

drawUI :: State -> [Widget n]
-- drawUI (State _ '\0') = [showMessage "Let's start!"]
drawUI (State _ 'w') = [showMessage "↑"]
drawUI (State _ 'a') = [showMessage "←"]
drawUI (State _ 's') = [showMessage "↓"]
drawUI (State _ 'd') = [showMessage "→"]
drawUI _ = [showMessage "Press arrow or wasd keys to move, ESC to quit"]

handleEvent :: BrickEvent n e -> EventM n State ()
handleEvent (VtyEvent (EvKey (KChar c) []))
  | c `elem` ['w', 'a', 's', 'd', 'q'] = updateCommand c
  | otherwise = updateCommand '?'
handleEvent (VtyEvent (EvKey KUp [])) = updateCommand 'w'
handleEvent (VtyEvent (EvKey KLeft [])) = updateCommand 'a'
handleEvent (VtyEvent (EvKey KDown [])) = updateCommand 's'
handleEvent (VtyEvent (EvKey KRight [])) = updateCommand 'd'
handleEvent (VtyEvent (EvKey KEsc [])) = halt

handleEvent (VtyEvent (EvKey _ [])) = updateCommand '?'
handleEvent ev = return ()

app :: App State e Name
app =
  App
    { appDraw = drawUI,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = showFirstCursor
    }

client :: ClientApp State
client conn = defaultMain app (State conn '?')

main :: IO ()
main = do
  putStrLn "Connecting server..."
  void $ runClient "0.0.0.0" 9160 "" client