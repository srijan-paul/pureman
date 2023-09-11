module Game.Food
  ( Food
  , drawFoods
  , foodSizePx
  , allFoods
  ) where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Game.Common (aligned, tileSize)
import Game.Maze (foodLocations, toRowCol)
import Game.State (State)
import Graphics.Canvas (Context2D, fillRect, setFillStyle)
import Undefined (undefined)

type Food =
  { row :: Int
  , col :: Int
  , eaten :: Boolean
  }

foodSizePx :: Number
foodSizePx = 4.0
