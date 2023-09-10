module Game.Pureman
  ( newGame
  ) where

import Prelude

import Data.Array (unsafeIndex)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Now as Now
import Effect.Ref as Ref
import Game.Common (Dir(..), tileSize)
import Game.Ghost.Update (drawGhost)
import Game.Graphics.Sprite (loadSpriteSheet)
import Game.Maze (Maze, TileKind(..), mapSize)
import Game.Player.Update (pacmanDraw)
import Game.State (Game, State, newState)
import Game.State.Update (stepState)
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
  go x =
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

changeDir :: Ref.Ref State -> Dir -> Effect Unit
changeDir stateRef dir = do
  state <- Ref.read stateRef
  when (state.pacman.moveDir /= dir) $
    Ref.modify_ (\s -> s { pacman = s.pacman { turnDir = dir } }) stateRef

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

update :: Number -> Ref.Ref State -> Effect Unit
update dt = Ref.modify_ $ stepState dt

draw :: Context2D -> Game -> Effect Unit
draw ctx game = do
  state <- Ref.read game.stateRef
  drawMaze game.atlas ctx state.maze
  pacmanDraw ctx state.pacman
  for_ state.ghosts $ drawGhost ctx

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
      stateRef <- Ref.new $ newState atlas
      let game = { stateRef, atlas }
      keypressListener <- eventListener $ handleKeyPress stateRef
      addEventListener (EventType "keydown") keypressListener false (toEventTarget doc)
      loop ctx win game time
