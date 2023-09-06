module Game.Common
  ( Dir(..)
  , aligned
  , collision
  , dir2Vector
  , oppositeDir
  , pair2Dir
  , tileSize
  ) where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Data.Number ((%))
import Game.Vec2 (Vec2, vec)

data Dir = Up | Left | Down | Right | None

derive instance eqDir :: Eq Dir

oppositeDir :: Dir -> Dir
oppositeDir dir = case dir of
  Left -> Right
  Right -> Left
  Up -> Down
  Down -> Up
  None -> None

tileSize :: Number
tileSize = 16.0

speed :: Number
speed = 1.0

collision :: Vec2 -> Vec2 -> Boolean
collision (x1 /\ y1) (x2 /\ y2) =
  x1 < x2 + tileSize
    && x1 + tileSize > x2
    && y1 < y2 + tileSize
    && y1 + tileSize > y2

dir2Vector :: Dir -> Vec2
dir2Vector dir = case dir of
  Up -> vec 0.0 (-speed)
  Down -> vec 0.0 speed
  Left -> vec (-speed) 0.0
  Right -> vec speed 0.0
  None -> vec 0.0 0.0

pair2Dir :: (Int /\ Int) -> Dir
pair2Dir pair = case pair of
  (1 /\ 0) -> Down
  (-1 /\ 0) -> Up
  (0 /\ 1) -> Right
  (0 /\ -1) -> Left
  _ -> None

aligned :: Number -> Boolean
aligned n = (n % tileSize) <= 0.5
