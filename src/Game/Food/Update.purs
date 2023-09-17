module Game.Food.Update where

import Prelude

import Data.Array (deleteAt, findIndex)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Game.Common (aligned, tileSize)
import Game.Food (Food, foodSizePx)
import Game.Maze (toRowCol)
import Game.State (State)
import Graphics.Canvas (Context2D, arc, fillPath, setFillStyle)

drawFoods :: Context2D -> Array Food -> Effect Unit
drawFoods ctx foods = do
  setFillStyle ctx "#b6ed77"
  for_ foods drawFood
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

updateFoods :: State -> Array Food -> (Int /\ Array Food)
updateFoods { pacman } foods =
  let
    pacmanPos = pacman.pos
    (pacRow /\ pacCol) = toRowCol pacmanPos
    isPacmanAligned = aligned (fst pacmanPos) && aligned (snd pacmanPos)
    touchesPacman food =
      food.row == pacRow && food.col == pacCol
  in
    if isPacmanAligned then
      let
        updatedFoods' = do
          index <- findIndex touchesPacman foods
          deleteAt index foods
      in
        case updatedFoods' of
          Just newFoods -> (10 /\ newFoods)
          Nothing -> (0 /\ foods)
    else (0 /\ foods)
