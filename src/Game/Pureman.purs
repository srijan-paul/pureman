module Game.Pureman
  ( Animation
  , Dir(..)
  , Frame
  , Maze
  , State
  , Pureman
  , TileKind(..)
  , drawAnimation
  , drawMaze
  , loadSpriteSheet
  , loop
  , mapSize
  , mazeString
  , newPacman
  , pacmanDraw
  , pacmanUpdate
  , parseMaze
  , stepAnimation
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.String as S
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Debug as Debug
import Effect (Effect)
import Game.Vec2 (Vec2, vec)
import Graphics.Canvas (CanvasImageSource, Context2D, drawImageFull, tryLoadImage)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)
import Web.HTML.Window (Window, requestAnimationFrame)

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

type Maze = Array (Array TileKind)
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
  coords _ = (528.0 /\ 24.0)

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

type Animation = { frames :: Array Frame, index :: Int, atlas :: CanvasImageSource, scale :: Number }

stepAnimation :: Animation -> Animation
stepAnimation anim@{ frames, index } =
  anim
    { index =
        if index + 1 < A.length frames then index + 1 else 0
    }

drawAnimation :: Context2D -> Animation -> Vec2 -> Effect Unit
drawAnimation ctx { frames, index, atlas, scale } (x /\ y) = do
  let maybeFrame = frames A.!! index
  for_ maybeFrame $ \frame -> do
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
  { animation: { frames, index: 0, atlas, scale: 2.0 }, pos: vec 0.0 0.0, w: 0.0, h: 0.0, moveDir: None }
  where
  frames =
    [ { x: 456.0, y: 0.0, w: 16.0, h: 16.0 }
    , { x: 472.0, y: 0.0, w: 16.0, h: 16.0 }
    , { x: 488.0, y: 0.0, w: 16.0, h: 16.0 }
    ]

pacmanUpdate :: Pureman -> Pureman
pacmanUpdate pureman@{ animation, pos, moveDir } =
  pureman { animation = stepAnimation animation, pos = updatedPos }
  where
  updatedPos = case moveDir of
    Up -> pos + (0.0 /\ -1.0)
    Down -> pos + (0.0 /\ 1.0)
    Left -> pos + (-1.0 /\ 0.0)
    Right -> pos + (1.0 /\ 0.0)
    None -> pos

pacmanDraw :: Pureman -> Context2D -> Effect Unit
pacmanDraw { animation, pos } ctx = drawAnimation ctx animation pos

type State =
  { pacman :: Pureman
  , maze :: Maze
  }

loop :: Context2D -> Window -> State -> Effect Unit
loop ctx window state = do
  let state' = state { pacman = pacmanUpdate state.pacman }
  pacmanDraw state'.pacman ctx
  void $ requestAnimationFrame (loop ctx window state') window
