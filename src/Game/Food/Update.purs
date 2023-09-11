module Game.Food.Update where

import Prelude

import Data.Array (filter)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Number (pi)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Game.Common (aligned, tileSize)
import Game.Food (Food, foodSizePx)
import Game.Maze (toRowCol)
import Game.State (State)
import Graphics.Canvas (Context2D, arc, fillPath, setFillStyle)

drawFoods :: Context2D -> Array Food -> Effect Unit
drawFoods ctx foods = do
  setFillStyle ctx "#b6ed77"
  for_ foods $ \food -> do
    when (not food.eaten) $ drawFood food
  where
  drawFood { row, col } =
    let
      x = (toNumber col) * tileSize + tileSize / 2.0
      y = (toNumber row) * tileSize + tileSize / 2.0
    in
      fillPath ctx $ arc ctx
        { x
        , y
        , radius: foodSizePx
        , start: 0.0
        , end: 2.0 * pi
        , useCounterClockwise: false
        }

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
