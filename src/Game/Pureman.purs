module Game.Pureman
  ( newGame
  ) where

import Prelude

import Data.Array (unsafeIndex)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number ((%))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Now as Now
import Effect.Ref as Ref
import Game.Animation (Animation, drawAnimation, stepAnimation)
import Game.Maze (Maze, TileKind(..), isWall, isWallAt, pacmanMaze, mapSize, tileAt, tileSize)
import Game.Sprite (PacmanAnims, loadSpriteSheet, makePacmanAnimations)
import Game.Vec2 (Vec2, vec)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImageFull, getContext2D)
import Partial.Unsafe (unsafePartial)
import Uitl (setImageSmoothing)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (Window, document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent as KE

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

  -- when (tile /= WallNone) $ do
  --   setStrokeStyle ctx "red"
  --   strokeRect ctx { x, y, width: tileSize - 2.0, height: tileSize - 2.0 }

  coords :: TileKind -> (Number /\ Number)
  coords WallTL = (16.0 /\ 16.0)
  coords WallTop = (24.0 /\ 16.0)
  coords WallTR = (40.0 /\ 16.0)
  coords WallBL = (16.0 /\ 32.0)
  coords WallBottom = (256.0 /\ 56.0)
  coords WallBR = (40.0 /\ 32.0)
  coords WallLeft = (16.0 /\ 24.0)
  coords WallRight = (40.0 /\ 24.0)
  coords WallNone = (0.0 /\ 80.0)

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
  , animations :: PacmanAnims
  }

newPacman :: CanvasImageSource -> Pureman
newPacman atlas =
  { animations
  , pos: vec (tileSize * 16.0) (tileSize * 23.0)
  , size: tileSize
  , moveDir: None
  , turnDir: None
  , animation: animations.right
  }

  where
  animations = makePacmanAnimations atlas

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
speed = 1.0

moveVector :: Dir -> Vec2
moveVector dir = case dir of
  Up -> vec 0.0 (-speed)
  Down -> vec 0.0 speed
  Left -> vec (-speed) 0.0
  Right -> vec speed 0.0
  None -> vec 0.0 0.0

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

dirToAnimation :: PacmanAnims -> Dir -> Animation
dirToAnimation anims dir = case dir of
  Left -> anims.left
  Right -> anims.right
  Up -> anims.up
  Down -> anims.down
  _ -> anims.right

pacmanUpdate :: Number -> State -> Pureman -> Pureman
pacmanUpdate dt { maze } pureman@{ animation, pos, moveDir, turnDir } =
  pureman
    { animation = stepAnimation dt animation'
    , pos = move moveDir'
    , moveDir = moveDir'
    , turnDir = turnDir'
    }
  where
  -- get current row-column from Pacman's x-y position
  (row /\ col) = toRowCol pos
  -- can we turn in the turnDir direction?
  canTurn =
    -- pacman cannot turn unless aligned to a grid cell
    if not $ aligned (fst pos) && aligned (snd pos) then false
    else
      let
        -- pacman cannot turn if the tile after turning is a wall
        nextRow /\ nextCol = turnRowCol row col turnDir
      in
        not $ isWallAt maze nextRow nextCol

  (moveDir' /\ turnDir' /\ animation') =
    if turnDir /= None && canTurn then (turnDir /\ None /\ (dirToAnimation pureman.animations turnDir))
    else (moveDir /\ turnDir /\ animation)

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

-- setStrokeStyle ctx "green"
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
  , maze: pacmanMaze
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
