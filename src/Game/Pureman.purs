module Game.Pureman (newGame) where

import Prelude

import Data.Array ((!!), unsafeIndex)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodeUnits (toCharArray)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Debug as Debug
import Effect (Effect)
import Effect.Now as Now
import Effect.Ref as Ref
import Game.Animation (Animation, drawAnimation, mkAnimation, stepAnimation)
import Game.Vec2 (Vec2, vec)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImageFull, getContext2D, setStrokeStyle, strokeRect, tryLoadImage)
import Partial.Unsafe (unsafePartial)
import Uitl (setImageSmoothing)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (Window, document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent as KE

mapSize :: (Int /\ Int)
mapSize = (30 /\ 32)

tileSize :: Number
tileSize = 16.0

loadSpriteSheet :: (Maybe CanvasImageSource -> Effect Unit) -> Effect Unit
loadSpriteSheet = tryLoadImage "spritesheet.png"

--- Representation of the PacMan level 0 maze as a string
--- Taken from the pacman example in https://fable.io/repl/
mazeString ∷ String
mazeString =
  """##/------------7/------------7##
##|............|!............|##
##|./__7./___7.|!./___7./__7.|##
##|o|  !.|   !.|!.|   !.|  !o|##
##|.L--J.L---J.LJ.L---J.L--J.|##
##|..........................|##
##|./__7./7./______7./7./__7.|##
##|.L--J.|!.L--7/--J.|!.L--J.|##
##|......|!....|!....|!......|##
##L____7.|L__7 |! /__J!./____J##
#######!.|/--J LJ L--7!.|#######
#######!.|!          |!.|#######
#######!.|! /__==__7 |!.|#######
-------J.LJ |      ! LJ.L-------
########.   | **** !   .########
_______7./7 |      ! /7./_______
#######!.|! L______J |!.|#######
#######!.|!          |!.|#######
#######!.|! /______7 |!.|#######
##/----J.LJ L--7/--J LJ.L----7##
##|............|!............|##
##|./__7./___7.|!./___7./__7.|##
##|.L-7!.L---J.LJ.L---J.|/-J.|##
##|o..|!.......<>.......|!..o|##
##L_7.|!./7./______7./7.|!./_J##
##/-J.LJ.|!.L--7/--J.|!.LJ.L-7##
##|......|!....|!....|!......|##
##|./____JL__7.|!./__JL____7.|##
##|.L--------J.LJ.L--------J.|##
##|..........................|##
##L--------------------------J##"""

data TileKind
  = Empty
  | WallLeft
  | WallRight
  | WallTop
  | WallBottom
  | WallTR
  | WallTL
  | WallBR
  | WallBL

derive instance eqTileKind :: Eq TileKind

type Maze = Array (Array TileKind)

infixl 5 unsafeIndex as <!!>

drawMaze :: CanvasImageSource -> Context2D -> Maze -> Effect Unit
drawMaze atlas ctx maze = do
  go 0 0
  where
  go :: Int -> Int -> Effect Unit
  go row col = do
    -- The number of rows and columns in the maze are constants.
    -- I don't want to play type tetris with a bunch of `Maybe`s for
    -- no good reason. This `unsafePartial` is justified, IMO.
    let
      tile = unsafePartial $ maze <!!> row <!!> col
      x = (toNumber col) * tileSize
      y = (toNumber row) * tileSize

    drawTile x y tile

    let
      (row' /\ col') =
        if col >= (snd mapSize) - 1 then ((row + 1) /\ 0)
        else (row /\ (col + 1))

    when (row' < fst mapSize) $ go row' col'

  drawTile :: Number -> Number -> TileKind -> Effect Unit
  drawTile x y tile = do
    let (tx /\ ty) = coords tile
    drawImageFull ctx atlas tx ty 8.0 8.0 x y tileSize tileSize

    when (tile /= Empty) $ do
      strokeRect ctx { x, y, width: tileSize - 2.0, height: tileSize - 2.0 }

  coords :: TileKind -> (Number /\ Number)
  coords WallTL = (16.0 /\ 16.0)
  coords WallTop = (24.0 /\ 16.0)
  coords WallTR = (40.0 /\ 16.0)
  coords WallBL = (16.0 /\ 32.0)
  coords WallBottom = (256.0 /\ 56.0)
  coords WallBR = (40.0 /\ 32.0)
  coords WallLeft = (16.0 /\ 24.0)
  coords WallRight = (40.0 /\ 24.0)
  coords Empty = (0.0 /\ 80.0)

parseMaze :: String -> Maze
parseMaze mazeS =
  map parseTile <<< toCharArray <$> rows
  where
  rows = S.split (S.Pattern "\n") mazeS
  parseTile c
    | c == '-' = WallTop
    | c == '_' = WallBottom
    | c == '/' = WallTL
    | c == '7' = WallTR
    | c == 'L' = WallBL
    | c == 'J' = WallBR
    | c == '|' = WallLeft
    | c == '!' = WallRight
    | otherwise = Empty

type Tile =
  { pos :: Vec2
  , size :: Number
  , kind :: TileKind
  }

tileAt :: Maze -> Int -> Int -> Maybe Tile
tileAt maze row col =
  ((maze !! row) >>= \r -> (r !! col)) <#>
    \k ->
      { pos: vec ((toNumber col) * tileSize) ((toNumber row) * tileSize)
      , size: tileSize
      , kind: k
      }

isWall :: TileKind -> Boolean
isWall Empty = false
isWall _ = true

data Dir = Up | Left | Down | Right | None

type Pureman =
  { animation :: Animation
  , moveDir :: Dir
  , pos :: Vec2
  , size :: Number -- hitbox size in pixels
  }

newPacman :: CanvasImageSource -> Pureman
newPacman atlas =
  { animation: mkAnimation atlas frames 120.0 1.8
  , pos: vec 256.0 368.0
  , size: tileSize
  , moveDir: None
  }
  where
  frames =
    [ { x: 456.0, y: 0.0, w: 16.0, h: 16.0 }
    , { x: 472.0, y: 0.0, w: 16.0, h: 16.0 }
    , { x: 488.0, y: 0.0, w: 16.0, h: 16.0 }
    ]

changeDir :: Ref.Ref State -> Dir -> Effect Unit
changeDir stateRef dir = do
  Ref.modify_ (\s -> s { pacman = s.pacman { moveDir = dir } }) stateRef

type AABB r =
  { pos :: Vec2
  , size :: Number
  | r
  }

collision :: forall r1 r2. AABB r1 -> AABB r2 -> Boolean
collision { pos: (x1 /\ y1), size: s1 } { pos: (x2 /\ y2), size: s2 } =
  x1 < x2 + s2
    && x1 + s1 > x2
    && y1 < y2 + s2
    && y1 + s1 > s2

pacmanUpdate :: Number -> State -> Pureman -> Pureman
pacmanUpdate dt { maze } pureman@{ animation, pos, moveDir, size } =
  pureman { animation = stepAnimation dt animation, pos = pos' }
  where
  tileRow = floor $ (snd pos) / tileSize
  tileCol = floor $ (fst pos) / tileSize
  -- find row column of tile we're facing
  (nextTileRow /\ nextTileCol) = case moveDir of
    Up -> ((tileRow - 1) /\ tileCol)
    Down -> ((tileRow + 1) /\ tileCol)
    Left -> (tileRow /\ (tileCol - 1))
    Right -> (tileRow /\ (tileCol + 1))
    None -> (tileRow /\ tileCol)
  -- What kind of tile are we facing? Is it a wall?
  nextTile = tileAt maze nextTileRow nextTileCol
  nextPos = case moveDir of
    Up -> pos + (0.0 /\ -1.0)
    Down -> pos + (0.0 /\ 1.0)
    Left -> pos + (-1.0 /\ 0.0)
    Right -> pos + (1.0 /\ 0.0)
    None -> pos

  pacmanHitbox = { pos: nextPos, size }

  pos' = case nextTile of
    Nothing -> nextPos
    Just tile' ->
      if isWall tile'.kind && collision pacmanHitbox tile' then pos
      else nextPos

handleKeyPress :: Ref.Ref State -> Event -> Effect Unit
handleKeyPress state event = do
  let maybeKey = KE.code <$> KE.fromEvent event
  for_ maybeKey $ \key -> do
    case key of
      "ArrowUp" -> changeDir state Up
      "ArrowDown" -> changeDir state Down
      "ArrowLeft" -> changeDir state Left
      "ArrowRight" -> changeDir state Right
      _ -> pure unit

pacmanDraw :: Context2D -> Pureman -> Effect Unit
pacmanDraw ctx { animation, pos, size } = do
  drawAnimation ctx animation (pos - ((size / 2.0) /\ (size / 2.0)))

-- setStrokeStyle ctx "red"
-- strokeRect ctx { x: fst pos, y: snd pos, width: size, height: size }

type State =
  { pacman :: Pureman
  , maze :: Maze
  }

type Game =
  { atlas :: CanvasImageSource
  , stateRef :: Ref.Ref State
  }

newState :: Pureman -> State
newState pacman =
  { pacman
  , maze: parseMaze mazeString
  }

stepState :: Number -> State -> State
stepState dt s@{ pacman } =
  s { pacman = pacmanUpdate dt s pacman }

update :: Number -> Ref.Ref State -> Effect Unit
update dt = Ref.modify_ $ stepState dt

draw :: Context2D -> Game -> Effect Unit
draw ctx game = do
  state <- Ref.read game.stateRef
  drawMaze game.atlas ctx state.maze
  pacmanDraw ctx state.pacman

loop :: Context2D -> Window -> Game -> Number -> Effect Unit
loop ctx window game prevMs = do
  Milliseconds currentMs <- Now.now >>= unInstant >>> pure
  let delta = currentMs - prevMs
  draw ctx game
  update delta game.stateRef
  void $ requestAnimationFrame (loop ctx window game currentMs) window

newGame :: Window -> CanvasElement -> Effect Unit
newGame win canvas = do
  (Milliseconds time) <- Now.now >>= unInstant >>> pure
  ctx <- getContext2D canvas
  doc <- document win
  setImageSmoothing ctx false
  loadSpriteSheet $ \maybeAtlas -> do
    for_ maybeAtlas \atlas -> do
      stateRef <- Ref.new $ newState (newPacman atlas)
      let game = { stateRef, atlas }
      keypressListener <- eventListener $ handleKeyPress stateRef
      addEventListener (EventType "keydown") keypressListener false (toEventTarget doc)
      loop ctx win game time
