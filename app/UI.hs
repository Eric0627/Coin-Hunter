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
import Data.Functor.Identity (Identity)
import Data.List (delete)
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

data Name
  = NGFNumRows
  | NGFNumCols
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
    _ngfAlgorithm :: Algorithm,
    _ngfSize :: Size
  }

makeLenses ''NewGameFormState

newGameForm :: NewGameFormState -> B.Form NewGameFormState e Name
newGameForm =
  B.newForm
    [ B.padBottom (B.Pad 1) . label ("# rows (<=" ++ show maxRows ++ "): ")
        B.@@= B.editShowableFieldWithValidate ngfNumRows NGFNumRows validRow,
      B.padBottom (B.Pad 1) . label ("# cols (<=" ++ show maxCols ++ "): ")
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
    validRow r = 1 <= r && r <= maxRows
    validCol c = 1 <= c && c <= maxCols

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
    _pCoins :: Int
  }

makeLenses ''Player

type Players = (Player, Player, Player, Player)

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

gameState ::
  StdGen ->
  Word32 ->
  Word32 ->
  Algorithm ->
  Size ->
  UTCTime ->
  UTCTime ->
  GameState
gameState _ numRows numCols _ _ _ _
  | numRows <= 0 = error "PANIC: gameState called with non-positive number of rows"
  | numCols <= 0 = error "PANIC: gameState called with non-positive number of columns"
gameState g numRows numCols alg size startTime currentTime =
  GameState maze players coinsPos monstersPos g3 ngf (GameMode InProgress NoDialog) startTime currentTime
  where
    (maze, g1) = case alg of
      RecursiveBacktracking -> recursiveBacktracking g numRows numCols
      BinaryTree -> binaryTree g numRows numCols
      Kruskal -> kruskal g numRows numCols
    (topLeft, _) = iMazeBounds maze
    player = Player topLeft 0
    players = (player, player, player, player)
    (coinsPos, g2) = sample g1 10 (iCoinCoords maze)
    (monstersPos, g3) = sample g2 5 (iCoinCoords maze)
    ngf = newGameForm (NewGameFormState numRows numCols alg size)

gsNewGame :: GameState -> GameState
gsNewGame gs = gameState g numRows numCols alg size st ct
  where
    g = gs ^. gsGen -- (^.) get gsGen in gs
    numRows = B.formState (gs ^. gsNewGameForm) ^. ngfNumRows
    numCols = B.formState (gs ^. gsNewGameForm) ^. ngfNumCols
    alg = B.formState (gs ^. gsNewGameForm) ^. ngfAlgorithm
    size = B.formState (gs ^. gsNewGameForm) ^. ngfSize
    st = gs ^. gsCurrentTime
    ct = gs ^. gsCurrentTime

mazeApp :: B.App GameState MazeEvent Name
mazeApp =
  B.App
    { B.appDraw = draw,
      B.appChooseCursor = \_ _ -> Nothing,
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
            B.hCenter $ status (gs ^. (gsGameMode . gmSolvingState)) (secondsElapsed gs) (gs ^. gsPlayers . _1 . pCoins)
          ],
      B.vLimit 3 $ B.center help
    ]

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
    playerPos = players ^. _1 . pPos
    maze = gs ^. gsMaze
    (_, bottomRight) = iMazeBounds maze
    dC = if isJust (iMazeMove maze coord DDown) then ' ' else '_'
    rC = if isJust (iMazeMove maze coord DRight) then ' ' else '|'
    lS = if col == 0 then "|" else ""
    tS
      | row /= 0 = ""
      | col /= 0 = "_ \n"
      | otherwise = " _ \n"
    isFinish = coord == bottomRight
    isSolved = playerPos == bottomRight
    isPlayerPos = playerPos == coord
    attr
      | isPlayerPos = B.attrName (if isFinish then "solved" else "pos")
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
    playerPos = gs ^. gsPlayers . _1 . pPos
    coinsPos = gs ^. gsCoinsPos
    monstersPos = gs ^. gsMonstersPos
    maze = gs ^. gsMaze
    (topLeft, bottomRight) = iMazeBounds maze
    tLeftBorder = if coord == topLeft then "┌" else ""
    tCenterBorder = if row == 0 then "───" else ""
    tRigthBorder = if row == 0 then (if col == 9 then "┐" else "─") else ""

    mLeftBorder = if col == 0 then "│" else ""
    m
      | coord == playerPos = " ⛑ "
      | coord `elem` monstersPos = " ⚉ "
      | coord `elem` coinsPos = " ◉ "
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

    isPlayerPos = coord == playerPos
    isStart = coord == topLeft
    isFinish = coord == bottomRight
    isCoin = coord `elem` coinsPos
    isMonster = coord `elem` monstersPos
    attr = case (isStart, isFinish, isPlayerPos, isCoin, isMonster) of
      (True, _, _, _, _) -> B.attrName "start"
      (_, True, True, _, _) -> B.attrName "solved"
      (_, True, False, _, _) -> B.attrName "finish"
      (False, False, True, _, _) -> B.attrName "pos"
      (False, False, False, True, False) -> B.attrName "coin"
      (False, False, False, _, True) -> B.attrName "monster"
      _ -> B.attrName "blank"

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
          [ B.str "up/down/left/right",
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
isSolved gs = (gs ^. gsPlayers . _1 . pPos) == snd (iMazeBounds $ gs ^. gsMaze)

-- isSolved _ = False -- for now

_i :: Int -> Lens' Players Player
_i 0 = _1
_i 1 = _2
_i 2 = _3
_i 3 = _4
_i _ = undefined

gsMove :: Int -> GameState -> Direction -> GameState
gsMove i gs dir = case nPos of
  Just nPos -> gs''
    where
      gs' = gsGetCoin' i (gs & gsPlayers .~ (players & _i i .~ (p & pPos .~ nPos)))
      gs'' = gs' & gsGameMode . gmSolvingState .~ (if isSolved gs' then Solved (secondsElapsed gs) coins else InProgress)
  Nothing -> gs
  where
    players = gs ^. gsPlayers
    p = players ^. _i i
    nPos = iMazeMove (gs ^. gsMaze) (p ^. pPos) dir
    coins = p ^. pCoins

gsGetCoin' :: Int -> GameState -> GameState
gsGetCoin' i gs = case (p ^. pPos) `elem` (gs ^. gsCoinsPos) of
  True -> gs''
    where
      gs' = gs & gsPlayers .~ (players & (_i i .~ (p & pCoins +~ 1)))
      gs'' = gs' & gsCoinsPos .~ ((p ^. pPos) `delete` (gs' ^. gsCoinsPos))
  False -> gs
  where
    p = gs ^. gsPlayers . _i i
    players = gs ^. gsPlayers

gsMoveMonsters :: GameState -> GameState
gsMoveMonsters gs = gs''
  where
    (dirs, newGen) = sample (gs ^. gsGen) 5 [DDown, DUp, DLeft, DRight]
    gs' = gs & gsGen .~ newGen
    gs'' = gs' & gsMonstersPos .~ zipWith moveMonster dirs (gs' ^. gsMonstersPos)
    moveMonster dir c
      | Just nPos <- iMazeMove (gs ^. gsMaze) c dir = nPos
      | otherwise = c

gsMeetMonster :: GameState -> GameState
gsMeetMonster gs
  | (gs ^. gsPlayers . _1 . pPos) `elem` (gs ^. gsMonstersPos) =
      gs & gsPlayers . _1 . pPos .~ snd (iMazeBounds (gs ^. gsMaze))
  | otherwise = gs

gsMove0 :: GameState -> Direction -> GameState
gsMove0 = gsMove 0

handleEvent :: B.BrickEvent Name MazeEvent -> B.EventM Name GameState ()
handleEvent event = do
  gs <- B.get
  case gs ^. (gsGameMode . gmDialog) of
    NoDialog -> case gs ^. gsGameMode . gmSolvingState of
      InProgress -> case event of
        -- Handle key events for player movement
        B.VtyEvent (V.EvKey V.KUp []) -> B.put (gsMove0 gs DUp)
        B.VtyEvent (V.EvKey V.KDown []) -> B.put (gsMove0 gs DDown)
        B.VtyEvent (V.EvKey V.KLeft []) -> B.put (gsMove0 gs DLeft)
        B.VtyEvent (V.EvKey V.KRight []) -> B.put (gsMove0 gs DRight)
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.put (gs & gsGameMode . gmDialog .~ NewGameDialog)
        -- Handle custom events
        B.AppEvent (Tick currentTime) -> B.put (gsMeetMonster gs & gsCurrentTime .~ currentTime)
        -- TODO: change gsMove0 to gsMove with index i
        B.AppEvent (ClientMove i dir) -> B.put (gsMove0 gs dir)
        B.AppEvent MonsterTick -> B.put (gsMoveMonsters gs)
        B.AppEvent QuitGame -> B.halt
        -- Other events and default
        _ -> B.put gs
      Solved _ _ -> case event of
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.put (gs & gsGameMode . gmDialog .~ NewGameDialog)
        B.AppEvent (Tick currentTime) -> B.put (gs & gsCurrentTime .~ currentTime)
        _ -> B.put gs
    NewGameDialog -> case event of
      B.VtyEvent (V.EvKey V.KEnter []) ->
        B.put (gsNewGame gs)
      B.VtyEvent (V.EvKey V.KEsc []) ->
        B.put (gs & gsGameMode . gmDialog .~ NoDialog)
      B.AppEvent (Tick currentTime) -> B.put (gs & gsCurrentTime .~ currentTime)
      _ -> B.put gs

attrMap :: GameState -> B.AttrMap
attrMap _ =
  B.attrMap
    V.defAttr
    [ (B.attrName "start", V.withForeColor V.defAttr V.blue),
      (B.attrName "finish", V.withBackColor V.defAttr V.red),
      (B.attrName "solved", V.withBackColor V.defAttr V.green),
      (B.attrName "blank", V.defAttr),
      (B.attrName "pos", V.withForeColor V.defAttr V.blue),
      (B.attrName "coin", V.withForeColor V.defAttr (V.rgbColor 255 215 0)),
      (B.attrName "monster", V.withForeColor V.defAttr V.red),
      (B.formAttr, V.defAttr),
      (B.editAttr, V.white `B.on` V.black),
      (B.editFocusedAttr, V.black `B.on` V.yellow),
      (B.focusedFormInputAttr, V.black `B.on` V.yellow),
      (B.invalidFormInputAttr, V.white `B.on` V.red)
    ]
