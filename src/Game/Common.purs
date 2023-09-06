module Game.Common
  ( Dir(..)
  , aligned
  , collision
  , dir2Vector
  , tileSize
  ) where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Number ((%))
import Game.Vec2 (Vec2, vec)

data Dir = Up | Left | Down | Right | None

derive instance eqDir :: Eq Dir

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

aligned :: Number -> Boolean
aligned n = (n % tileSize) <= 0.5
