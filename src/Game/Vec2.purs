module Game.Vec2
  ( Vec2
  , vec
  , dot
  ) where

import Prelude
import Data.Tuple.Nested ((/\), type (/\))

type Vec2 = (Number /\ Number)

vec :: Number -> Number -> Vec2
vec a b = (a /\ b)

dot :: Number -> Vec2 -> Vec2
dot k (x /\ y) = ((k * x) /\ (k * y))

infixr 5 dot as <.>