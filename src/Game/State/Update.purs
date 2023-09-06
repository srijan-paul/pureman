module Game.State.Update
  ( stepState
  ) where

import Prelude

import Game.Ghost.Update (updateGhost)
import Game.Player.Update (pacmanUpdate)
import Game.State (State)

stepState :: Number -> State -> State
stepState dt s@{ pacman, ghosts } =
  s
    { pacman = pacmanUpdate dt s pacman
    , ghosts = updateGhost dt s <$> ghosts
    }
