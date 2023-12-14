{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import qualified Brick as B
import qualified Brick.Forms as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import Control.Monad (unless, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor.Identity (Identity)
import Data.List (delete, elemIndex)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Word (Word32)
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Maze
import System.Random (StdGen)
import Text.Read (readMaybe)

maxRows :: Word32
maxRows = 20

maxCols :: Word32
maxCols = 40

maxPlayers :: Word32
maxPlayers = 10

data Name
  = NGFNumRows
  | NGFNumCols
  | NGFNPlayers
  | NGFRecursiveBacktracking
  | NGFBinaryTree
  | NGFKruskal
  | NGFBig
  | NGFSmall
  deriving (Show, Eq, Ord)

-- | The only additional event we use is a timer event from the outside world
-- telling us the current time so we can update the 'GameState'. It doesn't
-- matter how often these ticks are received, as long as they are requently
-- enough that we can know how many seconds has passed (so something like every
-- tenth of a second should be sufficient).
data MazeEvent = Tick UTCTime | ClientMove Int Direction | MonsterTick | QuitGame

data Dialog
  = NoDialog
  | NewGameDialog

data SolvingState
  = InProgress
  | Solved Int Int
  | NewGame

data GameMode = GameMode
  { _gmSolvingState :: SolvingState,
    _gmDialog :: Dialog
  }

makeLenses ''GameMode

data Algorithm
  = RecursiveBacktracking
  | BinaryTree
  | Kruskal
  deriving (Show, Eq, Ord)

data Size = Big | Small
  deriving (Show, Eq, Ord)

data NewGameFormState = NewGameFormState
  { _ngfNumRows :: Word32,
    _ngfNumCols :: Word32,
    _ngfNPlayers :: Word32,
    _ngfAlgorithm :: Algorithm,
    _ngfSize :: Size
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
        B.@@= B.editShowableFieldWithValidate ngfNumCols NGFNumCols validCol,
      B.padBottom (B.Pad 1) . label "algorithm: "
        B.@@= B.radioField
          ngfAlgorithm
          [ (RecursiveBacktracking, NGFRecursiveBacktracking, "recursive backtracking"),
            (BinaryTree, NGFBinaryTree, "binary tree"),
            (Kruskal, NGFKruskal, "kruskal's algorithm")
          ],
      label "size: "
        B.@@= B.radioField
          ngfSize
          [ (Big, NGFBig, "big"),
            (Small, NGFSmall, "small")
          ]
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
    _gsStartTime :: UTCTime,
    -- | Time right now
    _gsCurrentTime :: UTCTime
  }

makeLenses ''GameState

initGameState ::
  Word32 ->
  Word32 ->
  Word32 ->
  Algorithm ->
  Size ->
  StdGen ->
  UTCTime ->
  UTCTime ->
  GameState
initGameState numRows numCols n alg size g = GameState maze players coinsPos monstersPos g3 ngf (GameMode NewGame NewGameDialog)
  where
    (maze, g1) = case alg of
      RecursiveBacktracking -> recursiveBacktracking g numRows numCols
      BinaryTree -> binaryTree g numRows numCols
      Kruskal -> kruskal g numRows numCols
    (topLeft, _) = iMazeBounds maze
    (coinsPos, g2) = sample g1 10 (iCoinCoords maze)
    (monstersPos, g3) = sample g2 5 (iCoinCoords maze)
    players = replicate (fromIntegral n) (Player topLeft 0 False)
    ngf = newGameForm (NewGameFormState numRows numCols (fromIntegral n) alg size)

restartGame :: GameState -> IO GameState
restartGame gs = do
  st <- getCurrentTime

  return $ initGameState numRows numCols (fromIntegral n) alg size g st st
  where
    g = gs ^. gsGen -- (^.) get gsGen in gs
    gf = B.formState $ gs ^. gsNewGameForm
    alg = gf ^. ngfAlgorithm
    size = gf ^. ngfSize
    numRows = gf ^. ngfNumRows
    numCols = gf ^. ngfNumCols
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
    B.vLimit 30 $
      B.hLimit 50 $
        B.vBox
          [ B.borderWithLabel (B.str "New Game") $ B.renderForm (gs ^. gsNewGameForm),
            B.center $ B.str "(press \'return\' to start a new game or \'esc\' to cancel)"
          ]

drawMain :: GameState -> B.Widget n
drawMain gs =
  B.vBox
    [ B.vLimit 3 $ B.center $ B.str "Coin Hunter",
      B.center $
        B.vBox
          [ B.hCenter $ drawMaze gs,
            B.hCenter $ status state (secondsElapsed gs) coins0
          ],
      B.vLimit 3 $ B.center help
    ]
  where
    state = gs ^. gsGameMode . gmSolvingState
    coins0 = gs ^. gsPlayers . to head . pCoins

drawMaze :: GameState -> B.Widget n
drawMaze gs =
  B.vBox $
    (B.hBox . fmap (drawCell gs)) topRow
      : fmap (B.hBox . fmap (drawCell gs)) rows
  where
    (topRow : rows) = iMazeCoords (gs ^. gsMaze)
    drawCell = case B.formState (gs ^. gsNewGameForm) ^. ngfSize of
      Big -> drawCellBig
      Small -> drawCellSmall

drawCellSmall ::
  GameState ->
  -- | the cell to draw
  Coord ->
  B.Widget n
drawCellSmall gs coord =
  B.vBox
    [ B.str tS,
      B.hBox [B.str lS, B.withAttr attr (B.str [dC]), B.str [rC]]
    ]
  where
    (row, col) = (coordRow coord, coordCol coord)
    players = gs ^. gsPlayers
    playerPos0 = players ^. to head . pPos
    maze = gs ^. gsMaze
    (_, bottomRight) = iMazeBounds maze
    dC = if isJust (iMazeMove maze coord DDown) then ' ' else '─'
    rC = if isJust (iMazeMove maze coord DRight) then ' ' else '│'
    lS = if col == 0 then "│" else ""
    tS
      | row /= 0 = ""
      | col /= 0 = "─ \n"
      | otherwise = " ─ \n"
    isPlayerPos0 = playerPos0 == coord
    isFinish = coord == bottomRight
    attr
      | isPlayerPos0 = B.attrName (if isFinish then "solved" else "pos")
      | isFinish = B.attrName "finish"
      | otherwise = B.attrName "blank"

drawCellBig :: GameState -> Coord -> B.Widget n
drawCellBig gs coord =
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
    playerPos0 = gs ^. gsPlayers . to head . pPos
    coinsPos = gs ^. gsCoinsPos
    monstersPos = gs ^. gsMonstersPos
    maze = gs ^. gsMaze
    (topLeft, bottomRight) = iMazeBounds maze
    tLeftBorder = if coord == topLeft then "┌" else ""
    tCenterBorder = if row == 0 then "───" else ""
    tRigthBorder = if row == 0 then (if col == 9 then "┐" else "─") else ""

    mLeftBorder = if col == 0 then "│" else ""
    m
      | isMonster = " \986057 " -- 👾
      -- \| isMonster = " \983712 " -- 👻
      -- \| isMonster = " ⚉ "
      | isPlayerPos = " \986216 " -- 😀
      -- \| isPlayerPos = " ⛑ "
      | isCoin = " ◉ "
      | isFinish = " ⚐ "
      | otherwise = "   "

    isDownClear = isJust (iMazeMove maze coord DDown)
    isRightClear = isJust (iMazeMove maze coord DRight)

    mRightBorder = if isRightClear then "" else "│"

    bLeftBorder = if col == 0 then (if row == 9 then "└" else "│") else ""
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
            | row == 9 && col == 9 = "┘"
            | col == 9 = "┤"
            | row == 9 = "┴"
            | otherwise = "┼"
      (True, False, _, True) -> "│"
      (True, False, True, False) -> "└"
      (True, False, False, False) -> if col == 9 then "│" else "├"
      (_, _, False, _) -> "╷"
      (_, _, _, False) -> "╶"
      _ -> " "

    isPlayerPos = coord == playerPos0
    isStart = coord == topLeft
    isFinish = coord == bottomRight
    isCoin = coord `elem` coinsPos
    isMonster = coord `elem` monstersPos
    attr
      | isFinish = B.attrName "finish"
      | isMonster = B.attrName "monster"
      | isPlayerPos = B.attrName "pos"
      | isCoin = B.attrName "coin"
      | otherwise = B.attrName "blank"

secondsElapsed :: GameState -> Int
secondsElapsed gs =
  floor $
    nominalDiffTimeToSeconds $
      diffUTCTime (gs ^. gsCurrentTime) (gs ^. gsStartTime)

status :: SolvingState -> Int -> Int -> B.Widget n
status InProgress i j = B.str $ "Time: " ++ show i ++ "s" ++ " Coins: " ++ show j
status (Solved i j) _ _ = B.str $ "Solved in " ++ show i ++ "s with " ++ show j ++ " coins." ++ " Your total score is " ++ show (getScore i j) ++ ". Nice job!"

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
gsMove i gs dir = case nPos of
  Just nPos -> gs''
    where
      goal = snd (iMazeBounds $ gs ^. gsMaze)
      gs' = gsGetCoin i (gs & gsPlayers . ix i .~ p {_pPos = nPos, _pSolved = nPos == goal})
      coins = p ^. pCoins
      gs''
        | isSolved gs' = gs' & gsGameMode . gmSolvingState .~ Solved (secondsElapsed gs) coins
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
        B.AppEvent (ClientMove i dir) -> B.put $ gsMove0 gs dir
        B.AppEvent MonsterTick -> B.put $ gsMoveMonsters gs
        B.AppEvent QuitGame -> B.halt
        -- Other events and default
        _ -> return ()
      Solved _ _ -> case event of
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.put $ gs & gsGameMode . gmDialog .~ NewGameDialog
        B.AppEvent (Tick currentTime) -> B.put $ gs & gsCurrentTime .~ currentTime
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
