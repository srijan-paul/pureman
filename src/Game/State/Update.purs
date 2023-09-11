module Game.State.Update
  ( stepState
  ) where

import Prelude

import Game.Food.Update (updateFoods)
import Game.Ghost.Update (updateGhost)
import Game.Player.Update (pacmanUpdate)
import Game.State (State)

stepState :: Number -> State -> State
stepState dt s@{ pacman, ghosts, foods } =
  s
    { pacman = pacmanUpdate dt s pacman
    , ghosts = updateGhost dt s <$> ghosts
    , foods = updateFoods s foods
    }
