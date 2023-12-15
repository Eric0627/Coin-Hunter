{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import qualified Brick as B
import qualified Brick.BChan as B
import qualified Brick.Forms as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, modifyMVar, newMVar, threadDelay)
import Control.Monad (forM_, forever, unless, void, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (delete, elemIndex)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Word (Word32)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Platform.Unix as V
import Lens.Micro.Platform
import Maze
import Server (forkServer)
import System.Random (StdGen, getStdGen)
import Text.Read (readMaybe)

maxRows :: Word32
maxRows = 15

maxCols :: Word32
maxCols = 30

maxPlayers :: Word32
maxPlayers = 10

data Name
  = NGFNumRows
  | NGFNumCols
  | NGFNPlayers
  deriving (Show, Eq, Ord)

-- | The only additional event we use is a timer event from the outside world
-- telling us the current time so we can update the 'GameState'. It doesn't
-- matter how often these ticks are received, as long as they are requently
-- enough that we can know how many seconds has passed (so something like every
-- tenth of a second should be sufficient).
data Dialog
  = NoDialog
  | NewGameDialog

data SolvingState
  = InProgress
  | Solved
  | NewGame
  deriving (Eq)

data GameMode = GameMode
  { _gmSolvingState :: SolvingState,
    _gmDialog :: Dialog
  }

makeLenses ''GameMode

data NewGameFormState = NewGameFormState
  { _ngfNumRows :: Word32,
    _ngfNumCols :: Word32,
    _ngfNPlayers :: Word32
  }

makeLenses ''NewGameFormState

newGameForm :: NewGameFormState -> B.Form NewGameFormState e Name
newGameForm =
  B.newForm
    [ B.padBottom (B.Pad 1) . label "# players: "
        B.@@= B.editShowableFieldWithValidate ngfNPlayers NGFNPlayers validN,
      B.padBottom (B.Pad 1) . label ("# rows (3-" ++ show maxRows ++ "): ")
        B.@@= B.editShowableFieldWithValidate ngfNumRows NGFNumRows validRow,
      B.padBottom (B.Pad 1) . label ("# cols (3-" ++ show maxCols ++ "): ")
        B.@@= B.editShowableFieldWithValidate ngfNumCols NGFNumCols validCol
    ]
  where
    label s w = B.vLimit 1 (B.hLimit 15 $ B.str s B.<+> B.fill ' ') B.<+> w
    validN n = 1 <= n && n <= maxPlayers
    validRow r = 3 <= r && r <= maxRows
    validCol c = 3 <= c && c <= maxCols

-- | This will be merged into @brick@, so we can remove it at some point
editShowableFieldWithValidate ::
  (Ord n, Show n, Read a, Show a) =>
  -- | The state lens for this value.
  Lens' s a ->
  -- | The resource name for the input field.
  n ->
  -- | additional validation step for input.
  (a -> Maybe a) ->
  -- | The initial form state.
  s ->
  B.FormFieldState s e n
editShowableFieldWithValidate stLens n validate = B.editField stLens n limit ini val renderText id
  where
    ini = T.pack . show
    val = validate <=< (readMaybe . T.unpack . T.intercalate "\n")
    limit = Just 1
    renderText = B.txt . T.unlines

data Player = Player
  { _pPos :: Coord,
    _pCoins :: Int,
    _pScore :: Int,
    _pSolved :: Bool
  }

makeLenses ''Player

type Players = [Player]

data GameState = GameState
  { _gsMaze :: IMaze,
    -- , _gsPos :: Coord
    -- , _gsCoins :: Int
    _gsPlayers :: Players,
    _gsCoinsPos :: [Coord],
    _gsMonstersPos :: [Coord],
    _gsGen :: StdGen,
    _gsNewGameForm :: B.Form NewGameFormState MazeEvent Name,
    _gsGameMode :: GameMode,
    -- | Time when the current game was started
    _gsIDReseter :: IO (),
    _gsStartTime :: UTCTime,
    -- | Time right now
    _gsCurrentTime :: UTCTime
  }

makeLenses ''GameState

initGame ::
  Word32 ->
  Word32 ->
  Word32 ->
  StdGen ->
  IO () ->
  UTCTime ->
  UTCTime ->
  GameState
initGame numRows numCols n g = GameState maze players coinsPos monstersPos g3 ngf (GameMode NewGame NewGameDialog)
  where
    (maze, g1) = binaryTree g numRows numCols
    (topLeft, _) = iMazeBounds maze
    (coinsPos, g2) = sample g1 10 (iCoinCoords maze)
    (monstersPos, g3) = sample g2 5 (iCoinCoords maze)
    players = replicate (fromIntegral n) (Player topLeft 0 0 False)
    ngf = newGameForm (NewGameFormState numRows numCols (fromIntegral n))

restartGame :: GameState -> IO GameState
restartGame gs = do
  idReseter
  ct <- getCurrentTime

  return $ initGame numRows numCols (fromIntegral n) g idReseter ct ct
  where
    g = gs ^. gsGen -- (^.) get gsGen in gs
    gf = B.formState $ gs ^. gsNewGameForm
    numRows = gf ^. ngfNumRows
    numCols = gf ^. ngfNumCols
    idReseter = gs ^. gsIDReseter
    n = gf ^. ngfNPlayers

mazeApp :: B.App GameState MazeEvent Name
mazeApp =
  B.App
    { B.appDraw = draw,
      B.appChooseCursor = B.showFirstCursor,
      B.appHandleEvent = handleEvent,
      B.appStartEvent = return (),
      B.appAttrMap = attrMap
    }

draw :: GameState -> [B.Widget Name]
draw gs = case gs ^. (gsGameMode . gmDialog) of
  NoDialog -> [drawMain gs]
  NewGameDialog -> [drawNewGame gs]

drawNewGame :: GameState -> B.Widget Name
drawNewGame gs =
  B.center $
    B.vLimit 15 $
      B.hLimit 55 $
        B.vBox
          [ B.borderWithLabel (B.str "New Game") $ B.renderForm (gs ^. gsNewGameForm),
            B.center $ B.str "(press \'return\' to start a new game or \'esc\' to cancel)"
          ]

drawMain :: GameState -> B.Widget n
drawMain gs =
  B.vBox
    [ B.vLimit 3 $ B.center $ B.str "Coin Hunter",
      B.vBox
        [ B.hCenter $ drawMaze gs,
          B.hCenter $ status gs
        ],
      B.vLimit 5 $ B.center help
    ]

drawMaze :: GameState -> B.Widget n
drawMaze gs =
  B.vBox $
    (B.hBox . fmap (drawCell gs)) topRow
      : fmap (B.hBox . fmap (drawCell gs)) rows
  where
    (topRow : rows) = iMazeCoords (gs ^. gsMaze)

drawCell :: GameState -> Coord -> B.Widget n
drawCell gs coord =
  B.vBox
    [ B.hBox
        [ B.str tLeftBorder,
          B.str tCenterBorder,
          B.str tRigthBorder
        ],
      B.hBox
        [ B.str mLeftBorder,
          B.withAttr attr $ B.str m,
          B.str mRightBorder
        ],
      B.hBox
        [ B.str bLeftBorder,
          B.str bCenterBorder,
          B.str bRightBorder
        ]
    ]
  where
    (row, col) = (coordRow coord, coordCol coord)
    playerPos = map (\i -> gs ^. gsPlayers . to (!! i) . pPos) [0 .. length (gs ^. gsPlayers) - 1]
    coinsPos = gs ^. gsCoinsPos
    monstersPos = gs ^. gsMonstersPos
    maze = gs ^. gsMaze
    (topLeft, bottomRight) = iMazeBounds maze
    tLeftBorder = if coord == topLeft then "┌" else ""
    tCenterBorder = if row == 0 then "───" else ""
    tRigthBorder = if row == 0 then (if col == lastCol then "┐" else "─") else ""

    mLeftBorder = if col == 0 then "│" else ""
    playerIcons = [" \986216 ", " \986225 ", " \986219 ", " \983545 "]
    playerID = case elemIndex coord playerPos of
      Just index -> index
      Nothing -> -1
    m
      | isFinish = " ⚐ "
      | isMonster = " \986057 " -- 👾
      -- \| isMonster = " \983712 " -- 👻
      -- \| isMonster = " ⚉ "
      | playerID /= -1 = playerIcons !! playerID -- 😀
      -- \| isPlayerPos = " ⛑ "
      | isCoin = " ◉ "
      | otherwise = "   "

    isDownClear = isJust (iMazeMove maze coord DDown)
    isRightClear = isJust (iMazeMove maze coord DRight)

    mRightBorder = if isRightClear then "" else "│"

    bLeftBorder = if col == 0 then (if row == lastRow then "└" else "│") else ""
    bCenterBorder = if isDownClear then "   " else "───"

    downCoord = neighborCoord DDown coord
    rightCoord = neighborCoord DRight coord
    isDownRightClear = isJust (iMazeMove maze downCoord DRight)
    isRightDownClear = isJust (iMazeMove maze rightCoord DDown)

    bRightBorder = case (isDownClear, isRightClear, isDownRightClear, isRightDownClear) of
      (False, True, True, _) -> "─"
      (False, True, False, False) -> "─"
      (False, True, False, True) -> "┐"
      (False, False, True, True) -> "┘"
      (False, False, False, True) -> "┤"
      (False, False, True, False) -> "┴"
      (False, False, False, False) -> b
        where
          b
            | row == lastRow && col == lastCol = "┘"
            | col == lastCol = "┤"
            | row == lastRow = "┴"
            | otherwise = "┼"
      (True, False, _, True) -> "│"
      (True, False, True, False) -> "└"
      (True, False, False, False) -> if col == lastCol then "│" else "├"
      (_, _, False, _) -> "╷"
      (_, _, _, False) -> "╶"
      _ -> " "

    (lastRow, lastCol) = (coordRow bottomRight, coordCol bottomRight)

    isStart = coord == topLeft
    isFinish = coord == bottomRight
    isCoin = coord `elem` coinsPos
    isMonster = coord `elem` monstersPos
    attr
      | isFinish = B.attrName "finish"
      | isMonster = B.attrName "monster"
      | isCoin = B.attrName "coin"
      | otherwise = B.attrName "blank"

secondsElapsed :: GameState -> Int
secondsElapsed gs =
  floor $
    nominalDiffTimeToSeconds $
      diffUTCTime (gs ^. gsCurrentTime) (gs ^. gsStartTime)

status :: GameState -> B.Widget n
status gs
  | InProgress == gs ^. gsGameMode . gmSolvingState =
      B.str $ "Time: " ++ show (secondsElapsed gs) ++ "s" ++ foldr1 (++) (map (\i -> "\nPlayer " ++ show i ++ " Coins: " ++ show (gs ^. gsPlayers . to (!! i) . pCoins)) [0 .. length (gs ^. gsPlayers) - 1])
  | Solved == gs ^. gsGameMode . gmSolvingState =
      B.str $ "Solved in " ++ show (secondsElapsed gs) ++ "s." ++ foldr1 (++) (map (\i -> "\nPlayer " ++ show i ++ " Score: " ++ show (gs ^. gsPlayers . to (!! i) . pScore)) [0 .. length (gs ^. gsPlayers) - 1])
  | otherwise = B.str ""

getScore :: Int -> Int -> Int
getScore t c
  | t <= 30 = c + 3
  | t <= 45 = c + 2
  | t <= 60 = c + 1
  | otherwise = c

help :: B.Widget n
help =
  B.hBox
    [ B.padLeftRight 1 $
        B.vBox
          [ B.str "↑←↓→/wasd",
            B.str "n",
            B.str "q"
          ],
      B.padLeftRight 1 $
        B.vBox
          [ B.str "move position",
            B.str "new game",
            B.str "quit"
          ]
    ]

isSolved :: GameState -> Bool
isSolved gs = all (^. pSolved) (gs ^. gsPlayers)

gsMove :: Int -> GameState -> Direction -> GameState
gsMove i gs dir
  | i >= length players = gs
  | p ^. pSolved = gs
  | otherwise = case nPos of
      Just nPos -> gs''
        where
          goal = snd (iMazeBounds $ gs ^. gsMaze)
          score =
            if p ^. pScore == 0 && nPos == goal
              then getScore (secondsElapsed gs) (p ^. pCoins) + p ^. pCoins
              else 0
          gs' = gsGetCoin i (gs & gsPlayers . ix i .~ p {_pPos = nPos, _pSolved = nPos == goal, _pScore = score})
          coins = p ^. pCoins
          gs''
            | isSolved gs' = gs' & gsGameMode . gmSolvingState .~ Solved
            | nPos `elem` gs ^. gsMonstersPos = gs & gsPlayers . ix i . pPos .~ fst (iMazeBounds (gs ^. gsMaze))
            | otherwise = gs'
      Nothing -> gs
  where
    players = gs ^. gsPlayers
    p = players ^. to (!! i)
    nPos = iMazeMove (gs ^. gsMaze) (p ^. pPos) dir

gsGetCoin :: Int -> GameState -> GameState
gsGetCoin i gs =
  if (p ^. pPos) `elem` (gs ^. gsCoinsPos)
    then gs {_gsPlayers = players & ix i . pCoins +~ 1, _gsCoinsPos = (p ^. pPos) `delete` (gs ^. gsCoinsPos)}
    else gs
  where
    players = gs ^. gsPlayers
    p = players !! i

gsMoveMonsters :: GameState -> GameState
gsMoveMonsters gs = gs'
  where
    (dirs, newGen) = sample (gs ^. gsGen) 5 [DDown, DUp, DLeft, DRight]
    gs' = gs {_gsGen = newGen, _gsMonstersPos = zipWith moveMonster dirs (gs ^. gsMonstersPos)}
    players = gs ^. gsPlayers
    moveMonster dir oPos = case iMazeMove (gs ^. gsMaze) oPos dir of
      Just nPos
        | nPos `elem` map _pPos players -> oPos
        | otherwise -> nPos
      Nothing -> oPos

gsMeetMonster :: GameState -> GameState
gsMeetMonster gs = gs & gsPlayers .~ playerT `map` (gs ^. gsPlayers)
  where
    topLeft = fst (iMazeBounds (gs ^. gsMaze))
    playerT p = if (p ^. pPos) `elem` gs ^. gsMonstersPos then p & pPos .~ topLeft else p

gsMove0 :: GameState -> Direction -> GameState
gsMove0 = gsMove 0

handleEvent :: B.BrickEvent Name MazeEvent -> B.EventM Name GameState ()
handleEvent event = do
  gs <- B.get
  let mode = gs ^. gsGameMode
      solvingState = mode ^. gmSolvingState
  case mode ^. gmDialog of
    NoDialog -> case solvingState of
      InProgress -> case event of
        -- Handle key events for player movement
        B.VtyEvent (V.EvKey V.KUp []) -> B.put $ gsMove0 gs DUp
        B.VtyEvent (V.EvKey V.KLeft []) -> B.put $ gsMove0 gs DLeft
        B.VtyEvent (V.EvKey V.KDown []) -> B.put $ gsMove0 gs DDown
        B.VtyEvent (V.EvKey V.KRight []) -> B.put $ gsMove0 gs DRight
        B.VtyEvent (V.EvKey (V.KChar 'w') []) -> B.put $ gsMove0 gs DUp
        B.VtyEvent (V.EvKey (V.KChar 'a') []) -> B.put $ gsMove0 gs DLeft
        B.VtyEvent (V.EvKey (V.KChar 's') []) -> B.put $ gsMove0 gs DDown
        B.VtyEvent (V.EvKey (V.KChar 'd') []) -> B.put $ gsMove0 gs DRight
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.put $ gs & gsGameMode . gmDialog .~ NewGameDialog
        -- Handle custom events
        B.AppEvent (Tick currentTime) -> B.put $ gsMeetMonster gs & gsCurrentTime .~ currentTime
        B.AppEvent (ClientMove i dir) -> B.put $ gsMove i gs dir
        B.AppEvent MonsterTick -> B.put $ gsMoveMonsters gs
        B.AppEvent QuitGame -> B.halt
        -- Other events and default
        _ -> return ()
      Solved -> case event of
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.put $ gs & gsGameMode . gmDialog .~ NewGameDialog
        -- B.AppEvent (Tick currentTime) -> B.put $ gs & gsCurrentTime .~ currentTime
        _ -> return ()
      NewGame -> return () -- normally impossible
    NewGameDialog -> case event of
      B.VtyEvent (V.EvKey V.KEnter []) -> liftIO (restartGame gs) >>= B.put . (gsGameMode .~ GameMode InProgress NoDialog)
      B.VtyEvent (V.EvKey V.KEsc []) -> case solvingState of
        NewGame -> B.halt
        _ -> B.put $ gs & gsGameMode . gmDialog .~ NoDialog
      B.AppEvent (Tick currentTime) -> return ()
      _ -> zoom gsNewGameForm (B.handleFormEvent event)

attrMap :: GameState -> B.AttrMap
attrMap _ =
  B.attrMap
    V.defAttr
    [ (B.attrName "finish", V.withForeColor V.defAttr V.green),
      -- (B.attrName "solved", V.withBackColor V.defAttr V.green),
      (B.attrName "monster", V.withForeColor V.defAttr V.red),
      (B.attrName "pos", V.withForeColor V.defAttr V.blue),
      (B.attrName "coin", V.withForeColor V.defAttr (V.rgbColor 255 215 0)),
      (B.attrName "blank", V.defAttr),
      (B.formAttr, V.defAttr),
      (B.editAttr, V.white `B.on` V.black),
      (B.editFocusedAttr, V.black `B.on` V.yellow),
      (B.focusedFormInputAttr, V.black `B.on` V.yellow),
      (B.invalidFormInputAttr, V.white `B.on` V.red)
    ]

mazeGen :: IO ()
mazeGen = do
  eventChannel <- B.newBChan 10
  forkIO $
    forever $
      getCurrentTime
        >>= B.writeBChan eventChannel . Tick
        >> threadDelay 100000 -- tick every 0.1 seconds
  forkIO $
    forever $
      B.writeBChan eventChannel MonsterTick
        >> threadDelay 1000000 -- move monster every second
  g <- getStdGen
  st <- getCurrentTime

  reseter <- forkServer eventChannel

  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $
    B.customMain
      initialVty
      builder
      (Just eventChannel)
      mazeApp
      (initGame 10 10 1 g reseter st st)