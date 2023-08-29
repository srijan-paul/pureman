module Game.Pureman (newGame) where

import Prelude

import Data.Array as A
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.String as S
import Data.String.CodeUnits (toCharArray)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
import Game.Vec2 (Vec2, vec)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImageFull, getContext2D, rect, setFillStyle, setStrokeStyle, strokePath, tryLoadImage)
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

type Maze = Array (Array TileKind)

infixl 5 A.unsafeIndex as <!!>

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

-- A frame is an index into a spritesheet.
type Frame =
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }

type Animation =
  { frames :: Array Frame
  , index :: Int
  , atlas :: CanvasImageSource
  , scale :: Number
  , frameDuration :: Number
  , timeSinceLastFrame :: Number
  }

mkAnimation :: CanvasImageSource -> Array Frame -> Number -> Number -> Animation
mkAnimation atlas frames frameDuration scale =
  { frames
  , atlas
  , frameDuration
  , scale
  , index: 0
  , timeSinceLastFrame: 0.0
  }

stepAnimation :: Number -> Animation -> Animation
stepAnimation dt anim@{ frames, index, timeSinceLastFrame, frameDuration } =
  let
    shouldStep = timeSinceLastFrame + dt > frameDuration
  in
    if shouldStep then anim { timeSinceLastFrame = 0.0, index = stepIndex index }
    else anim { timeSinceLastFrame = timeSinceLastFrame + dt }

  where
  stepIndex idx = if idx + 1 < A.length frames then idx + 1 else 0

drawAnimation :: Context2D -> Animation -> Vec2 -> Effect Unit
drawAnimation ctx { frames, index, atlas, scale } (x /\ y) = do
  for_ (frames A.!! index) \frame -> do
    drawImageFull ctx atlas frame.x frame.y frame.w frame.h x y (frame.w * scale) (frame.h * scale)

data Dir = Up | Left | Down | Right | None

type Pureman =
  { animation :: Animation
  , moveDir :: Dir
  , pos :: Vec2
  , w :: Number -- hitbox width in pixels
  , h :: Number -- hitbox height in pixels
  }

newPacman :: CanvasImageSource -> Pureman
newPacman atlas =
  { animation: mkAnimation atlas frames 120.0 2.0
  , pos: vec 0.0 0.0
  , w: 0.0
  , h: 0.0
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

pacmanUpdate :: Number -> Pureman -> Pureman
pacmanUpdate dt pureman@{ animation, pos, moveDir } =
  pureman { animation = stepAnimation dt animation, pos = updatedPos }
  where
  updatedPos = case moveDir of
    Up -> pos + (0.0 /\ -1.0)
    Down -> pos + (0.0 /\ 1.0)
    Left -> pos + (-1.0 /\ 0.0)
    Right -> pos + (1.0 /\ 0.0)
    None -> pos

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
pacmanDraw ctx { animation, pos } = drawAnimation ctx animation pos

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

update :: Number -> Ref.Ref State -> Effect Unit
update dt =
  Ref.modify_ \s -> s { pacman = pacmanUpdate dt s.pacman }

draw :: Context2D -> State -> Effect Unit
draw ctx state = do
  pacmanDraw ctx state.pacman

loop :: Context2D -> Window -> Game -> Number -> Effect Unit
loop ctx window game prevMs = do
  Milliseconds currentMs <- Now.now >>= unInstant >>> pure
  let delta = currentMs - prevMs
  state <- Ref.read game.stateRef
  drawMaze game.atlas ctx state.maze
  draw ctx state
  update delta game.stateRef
  void $ requestAnimationFrame (loop ctx window game currentMs) window

newGame :: Window -> CanvasElement -> Effect Unit
newGame win canvas = do
  (Milliseconds time) <- Now.now >>= unInstant >>> pure
  ctx <- getContext2D canvas
  doc <- document win
  setImageSmoothing ctx false
  setStrokeStyle ctx "red"
  setFillStyle ctx "red"
  let path = rect ctx { x: 20.0, y: 20.0, width: 100.0, height: 100.0 }
  strokePath ctx path
  loadSpriteSheet $ \maybeAtlas -> do
    for_ maybeAtlas \atlas -> do
      stateRef <- Ref.new $ newState (newPacman atlas)
      let game = { stateRef, atlas }
      keypressListener <- eventListener $ handleKeyPress stateRef
      addEventListener (EventType "keydown") keypressListener false (toEventTarget doc)
      loop ctx win game time
