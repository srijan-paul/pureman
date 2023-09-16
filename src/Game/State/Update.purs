module Game.State.Update
  ( stepState
  ) where

import Prelude

import Data.Tuple.Nested ((/\))
import Game.Food.Update (updateFoods)
import Game.Ghost.Update (updateGhost)
import Game.Player.Update (pacmanUpdate)
import Game.State (State)

stepState :: Number -> State -> State
stepState dt s@{ pacman, ghosts, foods, score } =
  s
    { pacman = pacmanUpdate dt s pacman
    , ghosts = updateGhost dt s <$> ghosts
    , foods = foods'
    , score = score + dscore
    }
  where
  (dscore /\ foods') = updateFoods s foods
