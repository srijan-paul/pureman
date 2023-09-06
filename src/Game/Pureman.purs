module Game.Pureman
  ( newGame
  ) where

import Prelude

import Data.Array ((!!), unsafeIndex)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Number ((%))
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
import Undefined (undefined)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (Window, document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent as KE

mapSize :: (Int /\ Int)
mapSize = (31 /\ 32)

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
/------J.LJ |      ! LJ.L------7
|#######.   | **** !   .#######!
L______7./7 |      ! /7./______J
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
-- instance showTileKind :: Show TileKind where
--   show Empty = "Empty"
--   show WallLeft = "WallLeft"
--   show WallRight = "WallRight"
--   show WallTop = "WallTop"
--   show WallBottom = "WallBottom"
--   show WallTR = "WallTR"
--   show WallTL = "WallTL"
--   show WallBR = "WallBR"
--   show WallBL = "WallBL"

type Maze = Array (Array TileKind)

infixl 5 unsafeIndex as <!!>

forM_ :: forall m a. Monad m => Int -> Int -> (Int -> m a) -> m Unit
forM_ from to callback =
  go from
  where
  go x = do
    if (x < to) then do
      void $ callback x
      go (x + 1)
    else pure unit

drawMaze :: CanvasImageSource -> Context2D -> Maze -> Effect Unit
drawMaze atlas ctx maze = do
  let
    nRows = fst mapSize
    nCols = snd mapSize
  forM_ 0 nRows $ \row ->
    forM_ 0 nCols $ \col ->
      do
        -- log $ show (row /\ col)
        -- The number of rows and columns in the maze are constants.
        -- I don't want to play type tetris with a bunch of `Maybe`s for
        -- no good reason. This `unsafePartial` is justified, IMO.
        let
          tile = unsafePartial $ maze <!!> row <!!> col
          x = (toNumber col) * tileSize
          y = (toNumber row) * tileSize

        drawTile x y tile

  where
  drawTile :: Number -> Number -> TileKind -> Effect Unit
  drawTile x y tile = do
    let (tx /\ ty) = coords tile
    drawImageFull ctx atlas tx ty 8.0 8.0 x y tileSize tileSize

    when (tile /= Empty) $ do
      setStrokeStyle ctx "red"
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

isWallAt :: Maze -> Int -> Int -> Boolean
isWallAt maze row col = case ((maze !! row) >>= (\r -> r !! col)) of
  Just w -> isWall w
  Nothing -> false

data Dir = Up | Left | Down | Right | None

derive instance eqDir :: Eq Dir

type Pureman =
  { animation :: Animation
  -- current movement direction
  , moveDir :: Dir
  -- direction in which the player wants pacman to move next
  , turnDir :: Dir
  , pos :: Vec2
  , size :: Number -- hitbox size in pixels
  }

newPacman :: CanvasImageSource -> Pureman
newPacman atlas =
  { animation: mkAnimation atlas frames 120.0 1.8
  , pos: vec (tileSize * 16.0) (tileSize * 23.0)
  , size: tileSize
  , moveDir: None
  , turnDir: None
  }
  where
  frames =
    [ { x: 456.0, y: 0.0, w: 16.0, h: 16.0 }
    , { x: 472.0, y: 0.0, w: 16.0, h: 16.0 }
    , { x: 488.0, y: 0.0, w: 16.0, h: 16.0 }
    ]

changeDir :: Ref.Ref State -> Dir -> Effect Unit
changeDir stateRef dir = do
  Ref.modify_ (\s -> s { pacman = s.pacman { turnDir = dir } }) stateRef

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
    && y1 + s1 > y2

speed :: Number
speed = 0.5

moveVector :: Dir -> Vec2
moveVector Up = vec 0.0 (-speed)
moveVector Down = vec 0.0 speed
moveVector Left = vec (-speed) 0.0
moveVector Right = vec speed 0.0
moveVector None = vec 0.0 0.0

aligned :: Number -> Boolean
aligned n = (n % tileSize) <= 0.5

turnRowCol :: Int -> Int -> Dir -> (Int /\ Int)
turnRowCol r c dir =
  case dir of
    Up -> ((r - 1) /\ c)
    Down -> ((r + 1) /\ c)
    Left -> (r /\ (c - 1))
    Right -> (r /\ (c + 1))
    None -> (r /\ c)

toRowCol :: Vec2 -> (Int /\ Int)
toRowCol (x /\ y) = (floor $ y / tileSize) /\ (floor $ x / tileSize)

pacmanUpdate :: Number -> State -> Pureman -> Pureman
pacmanUpdate dt { maze } pureman@{ animation, pos, moveDir, turnDir } =
  pureman
    { animation = stepAnimation dt animation
    , pos = move moveDir'
    , moveDir = moveDir'
    , turnDir = turnDir'
    }
  where

  (row /\ col) = toRowCol pos
  -- can we turn in the turn direction?
  canTurn =
    -- pacman cannot turn unless aligned to a grid cell
    if not $ aligned (fst pos) && aligned (snd pos) then false
    else
      let
        -- pacman cannot turn if the tile after turning is a wall
        nextRow /\ nextCol = turnRowCol row col turnDir
      in
        not $ isWallAt maze nextRow nextCol

  (moveDir' /\ turnDir') =
    if turnDir /= None && canTurn then (turnDir /\ None)
    else (moveDir /\ turnDir)

  move :: Dir -> Vec2
  move dir =
    let
      dpos = moveVector dir
      newPos = pos + dpos
      newHitbox = { pos: newPos, size: tileSize }
      (row' /\ col') = turnRowCol row col dir
    in
      case tileAt maze row' col' of
        Just tile ->
          if (isWall tile.kind && collision newHitbox tile) then pos
          else newPos
        Nothing -> pos

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
  setStrokeStyle ctx "green"
  strokeRect ctx { x: fst pos, y: snd pos, width: size, height: size }

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
