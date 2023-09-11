module Game.Food.Update where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Game.Common (aligned, tileSize)
import Game.Food (Food, foodSizePx)
import Game.Maze (foodLocations, toRowCol)
import Game.State (State)
import Graphics.Canvas (Context2D, fillRect, setFillStyle)

drawFoods :: Context2D -> Array Food -> Effect Unit
drawFoods ctx foods = do
  setFillStyle ctx "yellow"
  for_ foods $ \food -> do
    when (not food.eaten) $ drawFood food
  where
  drawFood { row, col } =
    let
      x = (toNumber col) * tileSize + foodSizePx / 2.0
      y = (toNumber row) * tileSize + foodSizePx / 2.0
    in
      fillRect ctx { x, y, width: foodSizePx, height: foodSizePx }

updateFoods :: State -> Array Food -> Array Food
updateFoods { pacman } foods =
  let
    pacmanPos = pacman.pos
    (pacRow /\ pacCol) = toRowCol pacmanPos
    isPacmanAligned = aligned (fst pacmanPos) && aligned (snd pacmanPos)

    touchesPacman food =
      food.row == pacRow && food.col == pacCol
  in
    if isPacmanAligned then filter (not <<< touchesPacman) foods
    else foods

allFoods :: Array Food
allFoods =
  foodLocations <#> \(row /\ col) -> { row, col, eaten: false }