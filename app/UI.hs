{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Maze

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Forms as B
import Control.Monad ((<=<), unless)
import Data.Array.IArray
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Data.Time.Clock
import System.Random
import Data.Word
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.Set as Set
import Data.Maybe (isJust)

import Data.List

maxRows :: Word32
maxRows = 20

maxCols :: Word32
maxCols = 40

data Name = NGFNumRows
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

data MazeEvent = TimeTick UTCTime | MonsterTick UTCTime | Tick UTCTime | ClientMove Direction | Q

data Dialog = NoDialog
            | NewGameDialog

data SolvingState = InProgress
                  | Solved Int Int

data GameMode = GameMode { _gmSolvingState :: SolvingState
                         , _gmDialog :: Dialog
                         }
makeLenses ''GameMode

data Algorithm = RecursiveBacktracking
               | BinaryTree
               | Kruskal
  deriving (Show, Eq, Ord)

data Size = Big | Small
  deriving (Show, Eq, Ord)

data NewGameFormState = NewGameFormState
  { _ngfNumRows   :: Word32
  , _ngfNumCols   :: Word32
  , _ngfAlgorithm :: Algorithm
  , _ngfSize      :: Size
  }
makeLenses ''NewGameFormState

newGameForm :: NewGameFormState -> B.Form NewGameFormState e Name
newGameForm = B.newForm
  [ B.padBottom (B.Pad 1) . label ("# rows (<=" ++ show maxRows ++ "): ") B.@@=
    B.editShowableFieldWithValidate ngfNumRows NGFNumRows validRow
  , B.padBottom (B.Pad 1) . label ("# cols (<=" ++ show maxCols ++ "): ") B.@@=
    B.editShowableFieldWithValidate ngfNumCols NGFNumCols validCol
  , B.padBottom (B.Pad 1) . label "algorithm: " B.@@= B.radioField ngfAlgorithm
    [ (RecursiveBacktracking, NGFRecursiveBacktracking, "recursive backtracking")
    , (BinaryTree, NGFBinaryTree, "binary tree")
    , (Kruskal, NGFKruskal, "kruskal's algorithm")
    ]
  , label "size: " B.@@= B.radioField ngfSize
    [ (Big, NGFBig, "big")
    , (Small, NGFSmall, "small")
    ]
  ]
  where label s w =
          (B.vLimit 1 $ B.hLimit 15 $ B.str s B.<+> B.fill ' ') B.<+> w
        validRow r = 1 <= r && r <= maxRows
        validCol c = 1 <= c && c <= maxCols

-- | This will be merged into @brick@, so we can remove it at some point
editShowableFieldWithValidate :: (Ord n, Show n, Read a, Show a)
                   => Lens' s a
                   -- ^ The state lens for this value.
                   -> n
                   -- ^ The resource name for the input field.
                   -> (a -> Maybe a)
                   -- ^ additional validation step for input.
                   -> s
                   -- ^ The initial form state.
                   -> B.FormFieldState s e n
editShowableFieldWithValidate stLens n validate =
    let ini = T.pack . show
        val = validate <=< (readMaybe . T.unpack . T.intercalate "\n")
        limit = Just 1
        renderText = B.txt . T.unlines
    in B.editField stLens n limit ini val renderText id

data GameState = GameState
  { _gsMaze :: IMaze
  , _gsPos :: Coord
  , _gsCoins :: Int
  , _gsCoinsPos :: [Coord]
  , _gsMonstersPos :: [Coord]
  , _gsGen :: StdGen
  , _gsNewGameForm :: B.Form NewGameFormState MazeEvent Name
  , _gsGameMode :: GameMode
  , _gsStartTime :: UTCTime
    -- ^ Time when the current game was started
  , _gsCurrentTime :: UTCTime
    -- ^ Time right now
  }

makeLenses ''GameState

gameState :: StdGen
          -> Word32
          -> Word32
          -> Algorithm
          -> Size
          -> UTCTime
          -> UTCTime
          -> GameState
gameState _ numRows numCols _ _ _ _
  | numRows <= 0 = error "PANIC: gameState called with non-positive number of rows"
  | numCols <= 0 = error "PANIC: gameState called with non-positive number of columns"
gameState g numRows numCols alg size startTime currentTime =
  let (maze, g1) = case alg of
        RecursiveBacktracking -> recursiveBacktracking g numRows numCols
        BinaryTree            -> binaryTree g numRows numCols
        Kruskal               -> kruskal g numRows numCols
      ngf = newGameForm (NewGameFormState numRows numCols alg size)
      (topLeft, _) = iMazeBounds maze
      (coinsPos, g2) = sample g1 10 (iCoinCoords maze)
      (monstersPos, g3) = sample g2 5 (iCoinCoords maze)
  in GameState maze topLeft 0 coinsPos monstersPos g3 ngf (GameMode InProgress NoDialog) startTime currentTime

gsNewGame :: GameState -> GameState
gsNewGame gs = gameState g numRows numCols alg size st ct
  where g = gs ^. gsGen -- (^.) get gsGen in gs
        numRows = B.formState (gs ^. gsNewGameForm) ^. ngfNumRows
        numCols = B.formState (gs ^. gsNewGameForm) ^. ngfNumCols
        alg = B.formState (gs ^. gsNewGameForm) ^. ngfAlgorithm
        size = B.formState (gs ^. gsNewGameForm) ^. ngfSize
        st = gs ^. gsCurrentTime
        ct = gs ^. gsCurrentTime

mazeApp :: B.App GameState MazeEvent Name
mazeApp = B.App
  { B.appDraw = draw
  , B.appChooseCursor = \_ _ -> Nothing
  , B.appHandleEvent = handleEvent
  , B.appStartEvent = return ()
  , B.appAttrMap = attrMap
  }

draw :: GameState -> [B.Widget Name]
draw gs = case gs ^. gsGameMode ^. gmDialog of
  NoDialog -> [ drawMain gs ]
  NewGameDialog -> [ drawNewGame gs ]

drawNewGame :: GameState -> B.Widget Name
drawNewGame gs = B.center $ B.vLimit 30 $ B.hLimit 50 $
  B.vBox [ B.borderWithLabel (B.str "New Game") $ B.renderForm (gs ^. gsNewGameForm)
         , B.center $ B.str "(press \'return\' to start a new game or \'esc\' to cancel)"
         ]

drawMain :: GameState -> B.Widget n
drawMain gs = B.vBox
  [ B.vLimit 3 $ B.center $ B.str "Coin Hunter"
  , B.center $ B.vBox
    [ B.hCenter $ drawMaze gs
    , B.hCenter $ status (gs ^. gsGameMode ^. gmSolvingState) (secondsElapsed gs) (gs ^. gsCoins)
    ]
  , B.vLimit 3 $ B.center help
  ]

drawMaze :: GameState -> B.Widget n
drawMaze gs = B.vBox $
  (B.hBox . fmap (drawCell gs)) topRow :
  fmap (B.hBox . fmap (drawCell gs)) rows
  where (topRow:rows) = iMazeCoords (gs ^. gsMaze)
        drawCell = case B.formState (gs ^. gsNewGameForm) ^. ngfSize of
          Big -> drawCellBig
          Small -> drawCellSmall

drawCellSmall :: GameState
              -> Coord
              -- ^ the cell to draw
              -> B.Widget n
drawCellSmall gs coord = B.vBox
  [ B.str $ tS
  , B.hBox [B.str lS, B.withAttr attr (B.str [dC]), B.str [rC]]
  ]
  where (row, col) = (coordRow coord, coordCol coord)
        playerPos = gs ^. gsPos
        maze = gs ^. gsMaze
        (_, bottomRight) = iMazeBounds maze
        dC = if isJust (iMazeMove maze coord DDown) then ' ' else '_'
        rC = if isJust (iMazeMove maze coord DRight) then ' ' else '|'
        lS = if col == 0 then "|" else ""
        tS = if row == 0
             then if col == 0 then " _ \n" else "_ \n"
             else ""
        isFinish = coord == bottomRight
        isSolved = playerPos == bottomRight
        isPlayerPos = playerPos == coord
        attr | isPlayerPos = B.attrName (if isFinish then "solved" else "pos")
             | isFinish = B.attrName "finish"
             | otherwise = B.attrName "blank"

drawCellBig :: GameState -> Coord -> B.Widget n
drawCellBig gs coord = B.vBox
  [ B.str $ topLeftBorder ++ topBorder
  , B.hBox
    [ B.str leftBorder
    , B.str " "
    , B.withAttr attr $ B.str [m]
    , B.str " "
    , B.str [r]
    ]
  , B.hBox
    [ B.str leftBorder
    , B.str [d , d , d]
    , B.str [br]
    ]
  ]
  where (row, col) = (coordRow coord, coordCol coord)
        playerPos = gs ^. gsPos
        coinsPos = gs ^. gsCoinsPos
        monstersPos = gs ^. gsMonstersPos
        maze = gs ^. gsMaze
        (topLeft, bottomRight) = iMazeBounds maze
        m | coord == playerPos = '*'
          | elem coord monstersPos = 'x'
          | elem coord coinsPos = '$'
          | otherwise = ' '
        d = if isJust (iMazeMove maze coord DDown) then ' ' else '_'
        r = if isJust (iMazeMove maze coord DRight) then ' ' else '|'
        br = if isJust (iMazeMove maze coord DRight) then ' ' else '|'

        leftBorder = if col == 0 then "|" else ""
        topBorder = if row == 0 then "___ " else ""
        topLeftBorder = if coord == topLeft then " " else ""

        isPlayerPos = coord == playerPos
        isStart = coord == topLeft
        isFinish = coord == bottomRight
        isCoin = elem coord coinsPos
        isMonster = elem coord monstersPos
        attr = case (isStart, isFinish, isPlayerPos, isCoin, isMonster) of
          (True, _, _, _, _) -> B.attrName "start"
          (_, True, True, _, _) -> B.attrName "solved"
          (_, True, False, _, _) -> B.attrName "finish"
          (False, False, True, _, _) -> B.attrName "pos"
          (False, False, False, True, False) -> B.attrName "coin"
          (False, False, False, _, True) -> B.attrName "monster"
          _ -> B.attrName "blank"

secondsElapsed :: GameState -> Int
secondsElapsed gs = floor $ nominalDiffTimeToSeconds $
  diffUTCTime (gs ^. gsCurrentTime) (gs ^. gsStartTime)

status :: SolvingState -> Int -> Int -> B.Widget n
status InProgress i j = B.str $ "Time: " ++ show i ++ "s" ++ " Coins: " ++ show j
status (Solved i j) _ _ = B.str $ "Solved in " ++ show i ++ "s with " ++ show j ++ " coins! Nice job!"

help :: B.Widget n
help = B.hBox
  [ B.padLeftRight 1 $
    B.vBox [ B.str "up/down/left/right"
           , B.str "n"
           , B.str "q"
           ]
  , B.padLeftRight 1 $
    B.vBox [ B.str "move position"
           , B.str "new game"
           , B.str "quit"
           ]
  ]

isSolved :: GameState -> Bool
isSolved gs = let (_, bottomRight) = iMazeBounds (gs ^. gsMaze)
              in gs ^. gsPos == bottomRight

gsMove :: GameState -> Direction -> GameState
gsMove gs0 dir
  | Just nPos <- iMazeMove (gs0 ^. gsMaze) (gs0 ^. gsPos) dir =
    let gs1 = gs0 & gsPos .~ nPos -- (.~) replace gsPos in gs0 with nPos
        gs2 = gsGetCoin gs1
        gs3 = gs2 & gsGameMode . gmSolvingState .~ case isSolved gs2 of
          True -> Solved (secondsElapsed gs0) (gs3 ^. gsCoins)
          False -> InProgress
    in gs3
  | otherwise = gs0


gsGetCoin :: GameState -> GameState
gsGetCoin gs0 =
  if (gs0 ^. gsPos) `elem` (gs0 ^. gsCoinsPos) then
    let gs1 = gs0 & gsCoins .~ (gs0 ^. gsCoins + 1)
        gs2 = gs1 & gsCoinsPos .~ ((gs0 ^. gsPos) `delete` (gs1 ^. gsCoinsPos))
    in gs2
  else gs0

gsMoveMonsters :: GameState -> GameState
gsMoveMonsters gs0 = 
    let (dirs, newGen) = sample (gs0 ^. gsGen) 5 [DDown, DUp, DLeft, DRight]
        gs1 = gs0 & gsGen .~ newGen
        gs2 = gs1 & gsMonstersPos .~ (zipWith moveMonster dirs (gs1 ^. gsMonstersPos)) -- (.~) replace gsPos in gs0 with nPos
    in gs2
  where moveMonster dir c
          | Just nPos <- iMazeMove (gs0 ^. gsMaze) c dir =
            nPos
          | otherwise = c

handleEvent :: B.BrickEvent Name MazeEvent -> B.EventM Name GameState ()
handleEvent event = do
  gs <- B.get
  case gs ^. gsGameMode ^. gmDialog of
    NoDialog -> case gs ^. gsGameMode . gmSolvingState of
      InProgress -> case event of
        -- Handle key events for player movement
        B.VtyEvent (V.EvKey V.KUp []) -> B.put (gsMove gs DUp)
        B.VtyEvent (V.EvKey V.KDown []) -> B.put (gsMove gs DDown)
        B.VtyEvent (V.EvKey V.KLeft []) -> B.put (gsMove gs DLeft)
        B.VtyEvent (V.EvKey V.KRight []) -> B.put (gsMove gs DRight)
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) ->
              B.put (gs & gsGameMode . gmDialog .~ NewGameDialog)
        -- Handle custom ClientMove event from the server
        B.AppEvent (ClientMove dir) -> B.put (gsMove gs dir)
        B.AppEvent Q -> B.halt
        -- B.VtyEvent (V.EvKey (V.KChar 'n') []) ->
        --       B.put (gs & gsGameMode . gmDialog .~ NewGameDialog)

        -- Handle Tick event for updating the game state based on time
        B.AppEvent (TimeTick currentTime) -> B.put (gs & gsCurrentTime .~ currentTime)                             
        B.AppEvent (MonsterTick currentTime) -> B.put (gsMoveMonsters gs)

        -- Other events and default
        _ -> B.put gs
      Solved _ _ -> case event of
        B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt
        B.VtyEvent (V.EvKey (V.KChar 'n') []) ->
          B.put (gs & gsGameMode . gmDialog .~ NewGameDialog)
        B.AppEvent (TimeTick currentTime) -> B.put (gs & gsCurrentTime .~ currentTime)
        _ -> B.put gs
    NewGameDialog -> case event of
      B.VtyEvent (V.EvKey V.KEnter []) ->
        B.put (gsNewGame gs)
      B.VtyEvent (V.EvKey V.KEsc []) ->
        B.put (gs & gsGameMode . gmDialog .~ NoDialog)
      B.AppEvent (Tick currentTime) -> B.put (gs & gsCurrentTime .~ currentTime)
      _ -> B.put gs
      -- _ -> do f' <- B.handleFormEvent event (gs ^. gsNewGameForm)
      --         B.put (gs & gsNewGameForm .~ f')

attrMap :: GameState -> B.AttrMap
attrMap _ = B.attrMap V.defAttr
  [ (B.attrName "start", V.withForeColor V.defAttr V.blue)
  , (B.attrName "finish", V.withBackColor V.defAttr V.red)
  , (B.attrName "solved", V.withBackColor V.defAttr V.green)
  , (B.attrName "blank", V.defAttr)
  , (B.attrName "pos", V.withForeColor V.defAttr V.blue)
  , (B.attrName "coin", V.withForeColor V.defAttr (V.rgbColor 255 215 0))
  , (B.attrName "monster", V.withForeColor V.defAttr V.red)
  , (B.formAttr, V.defAttr)
  , (B.editAttr, V.white `B.on` V.black)
  , (B.editFocusedAttr, V.black `B.on` V.yellow)
  , (B.focusedFormInputAttr, V.black `B.on` V.yellow)
  , (B.invalidFormInputAttr, V.white `B.on` V.red)
  ]
