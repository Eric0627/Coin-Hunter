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
import Data.List (delete)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Word (Word32)
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Maze
import System.Random (StdGen, getStdGen)
import Text.Read (readMaybe)

maxRows :: Word32
maxRows = 30

maxCols :: Word32
maxCols = 50

maxPlayers :: Word32
maxPlayers = 4

numCoins :: Int
numCoins = 30

numMonsters :: Int
numMonsters = 30

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
-- telling us the current time so we can update the 'gameState. It doesn't
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
  | MyAlgorithm
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

-- type Players = (Player, Player, Player, Player)

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
      MyAlgorithm -> myAlgorithm g numRows numCols
    (topLeft, _) = iMazeBounds maze
    (coinsPos, g2) = sample g1 numCoins (iCoinCoords maze)
    (monstersPos, g3) = sample g2 numMonsters (iCoinCoords maze)
    players = replicate (fromIntegral n) (Player (iMazeEntranceCoord maze) 0 False)
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

drawCell :: GameState -> Coord -> B.Widget n
drawCell gs coord =
  B.hBox
    [ B.withAttr attr $ B.str m]
  where
    (row, col) = (coordRow coord, coordCol coord)
    playerPos0 = gs ^. gsPlayers . to head . pPos
    coinsPos = gs ^. gsCoinsPos
    monstersPos = gs ^. gsMonstersPos
    maze = gs ^. gsMaze
    cell = iMazeGetCell maze coord
    (topLeft, bottomRight) = iMazeBounds maze
    m
      | coord == playerPos0 = "**"
      | coord `elem` monstersPos = "xx"
      | coord `elem` coinsPos = "$$"
      | isWall cell = "  "
      | otherwise = "  "
    isBlocked = isWall cell
    isPlayerPos = coord == playerPos0
    isStart = coord == getCoord (maxRows `div` 2) 0
    isFinish = coord == getCoord (maxRows `div` 2) (maxCols-1)
    isCoin = coord `elem` coinsPos
    isMonster = coord `elem` monstersPos
    attr = case (isStart, isFinish, isPlayerPos, isCoin, isMonster, isBlocked) of
      (True, _, _, _, _, False) -> B.attrName "start"
      (_, True, True, _, _, False) -> B.attrName "solved"
      (_, True, False, _, _, False) -> B.attrName "finish"
      (False, False, True, _, _, False) -> B.attrName "pos"
      (False, False, False, True, False, False) -> B.attrName "coin"
      (False, False, False, _, True, False) -> B.attrName "monster"
      (False, False, False, False, False, True) -> B.attrName "wall"
      _ -> B.attrName "blank"

secondsElapsed :: GameState -> Int
secondsElapsed gs =
  floor $
    nominalDiffTimeToSeconds $
      diffUTCTime (gs ^. gsCurrentTime) (gs ^. gsStartTime)

status :: SolvingState -> Int -> Int -> B.Widget n
status InProgress i j = B.str $ "Time: " ++ show i ++ "s" ++ " Coins: " ++ show j
status (Solved i j) _ _ = B.str $ "Solved in " ++ show i ++ "s with " ++ show j ++ " coins! Nice job!"
status NewGame _ _ = B.str "New game. Like how are you even seeing this?"

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
      --goal = snd (iMazeBounds $ gs ^. gsMaze)
      gs' = gsGetCoin i (gs & gsPlayers .~ (players & ix i .~ p {_pPos = nPos, _pSolved = nPos == getCoord (maxRows `div` 2) (maxCols-1)}))
      coins = p ^. pCoins
      gs'' = (if isSolved gs' then gsGameMode . gmSolvingState .~ Solved (secondsElapsed gs) coins else id) gs'
  Nothing -> gs
  where
    players = gs ^. gsPlayers
    p = players ^. to (!! i)
    nPos = iMazeMove (gs ^. gsMaze) (p ^. pPos) dir

gsGetCoin :: Int -> GameState -> GameState
gsGetCoin i gs = case (p ^. pPos) `elem` (gs ^. gsCoinsPos) of
  True -> gs''
    where
      gs' = gs & gsPlayers .~ (players & (ix i .~ (p & pCoins +~ 1)))
      gs'' = gs' & gsCoinsPos .~ ((p ^. pPos) `delete` (gs' ^. gsCoinsPos))
  False -> gs
  where
    players = gs ^. gsPlayers
    p = players !! i

gsMoveMonsters :: GameState -> GameState
gsMoveMonsters gs = gs''
  where
    (dirs, newGen) = sample (gs ^. gsGen) numMonsters [DDown, DUp, DLeft, DRight]
    gs' = gs & gsGen .~ newGen
    gs'' = gs' & gsMonstersPos .~ zipWith moveMonster dirs (gs' ^. gsMonstersPos)
    moveMonster dir c
      | Just nPos <- iMazeMove (gs ^. gsMaze) c dir = nPos
      | otherwise = c

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
        B.VtyEvent (V.EvKey V.KUp []) -> B.put (gsMove0 gs DUp)
        B.VtyEvent (V.EvKey V.KLeft []) -> B.put (gsMove0 gs DLeft)
        B.VtyEvent (V.EvKey V.KDown []) -> B.put (gsMove0 gs DDown)
        B.VtyEvent (V.EvKey V.KRight []) -> B.put (gsMove0 gs DRight)
        B.VtyEvent (V.EvKey (V.KChar 'w') []) -> B.put (gsMove0 gs DUp)
        B.VtyEvent (V.EvKey (V.KChar 's') []) -> B.put (gsMove0 gs DDown)
        B.VtyEvent (V.EvKey (V.KChar 'a') []) -> B.put (gsMove0 gs DLeft)
        B.VtyEvent (V.EvKey (V.KChar 'd') []) -> B.put (gsMove0 gs DRight)
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.put (gs & gsGameMode . gmDialog .~ NewGameDialog)
        -- Handle custom events
        B.AppEvent (Tick currentTime) -> B.put (gs & gsCurrentTime .~ currentTime)
        B.AppEvent (ClientMove i dir) -> B.put (gsMove0 gs dir)
        B.AppEvent MonsterTick -> B.put (gsMoveMonsters gs)
        B.AppEvent QuitGame -> B.halt
        -- Other events and default
        _ -> return ()
      Solved _ _ -> case event of
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.put (gs & gsGameMode . gmDialog .~ NewGameDialog)
        B.AppEvent (Tick currentTime) -> B.put (gs & gsCurrentTime .~ currentTime)
        _ -> return ()
      NewGame -> return () -- normally impossible
    NewGameDialog -> case event of
      B.VtyEvent (V.EvKey V.KEnter []) -> liftIO (restartGame gs) >>= B.put . (gsGameMode .~ GameMode InProgress NoDialog)
      B.VtyEvent (V.EvKey V.KEsc []) -> case solvingState of
        NewGame -> B.halt
        _ -> B.put (gs & gsGameMode . gmDialog .~ NoDialog)
      B.AppEvent (Tick currentTime) -> return ()
      _ -> zoom gsNewGameForm (B.handleFormEvent event)

attrMap :: GameState -> B.AttrMap
attrMap _ = B.attrMap V.defAttr
  [ (B.attrName "start", V.withForeColor V.defAttr V.blue)
  , (B.attrName "finish", V.withBackColor V.defAttr V.red)
  , (B.attrName "solved", V.withBackColor V.defAttr V.green)
  , (B.attrName "blank", V.defAttr)
  , (B.attrName "pos", V.withForeColor V.defAttr V.blue)
  , (B.attrName "coin", V.withForeColor V.defAttr (V.rgbColor 255 215 0))
  , (B.attrName "monster", V.withForeColor V.defAttr V.red)
  , (B.attrName "wall", V.withBackColor V.defAttr V.cyan)
  , (B.formAttr, V.defAttr)
  , (B.editAttr, V.white `B.on` V.black)
  , (B.editFocusedAttr, V.black `B.on` V.yellow)
  , (B.focusedFormInputAttr, V.black `B.on` V.yellow)
  , (B.invalidFormInputAttr, V.white `B.on` V.red)
  ]
