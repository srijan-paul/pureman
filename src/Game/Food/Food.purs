module Game.Food
  ( Food
  , allFoods
  , foodSizePx
  ) where

import Prelude

import Game.Maze (foodLocations)
import Data.Tuple.Nested ((/\))

type Food =
  { row :: Int
  , col :: Int
  }

foodSizePx :: Number
foodSizePx = 3.0

allFoods :: Array Food
allFoods =
  foodLocations <#> \(row /\ col) -> { row, col }
