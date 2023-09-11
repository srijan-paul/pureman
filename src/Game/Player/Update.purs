module Game.Player.Update
  ( pacmanDraw
  , pacmanUpdate
  ) where

import Prelude

import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Game.Common (Dir(..), tileSize, aligned)
import Game.Entity (move)
import Game.Graphics.Animation (drawAnimation, stepAnimation)
import Game.Graphics.Sprite (dirToAnimation)
import Game.Maze (isWallAt, toRowCol, turnRowCol)
import Game.Player.Pacman (Pacman)
import Game.State (State)
import Graphics.Canvas (Context2D)

pacmanDraw :: Context2D -> Pacman -> Effect Unit
pacmanDraw ctx { animation, pos } = do
  drawAnimation ctx animation (pos - ((tileSize / 2.0) /\ (tileSize / 2.0)))

pacmanUpdate :: Number -> State -> Pacman -> Pacman
pacmanUpdate dt { maze } pureman@{ animation, pos, moveDir, turnDir } =
  pureman
    { animation = stepAnimation dt animation'
    , pos = pos'
    , moveDir = moveDir'
    , turnDir = turnDir'
    }
  where
  -- get current row-column from Pacman's x-y position
  (row /\ col) = toRowCol pos
  -- can we turn in the turnDir direction?
  canTurn =
    -- pacman cannot turn unless aligned to a grid cell
    if not $ aligned (fst pos) && aligned (snd pos) then false
    else
      let
        -- pacman cannot turn if the tile after turning is a wall
        nextRow /\ nextCol = turnRowCol row col turnDir
      in
        not $ isWallAt maze nextRow nextCol

  -- compute the next move/turn directions and animation.
  (moveDir' /\ turnDir' /\ animation') =
    -- If the player has issued a turn command and we're at a position
    -- where making a turn is possible, then change direction.
    if turnDir /= None && canTurn then
      (turnDir /\ None /\ (dirToAnimation pureman.animations turnDir))
    else
      -- Otherwise, continue moving in the direction we're currently facing.
      (moveDir /\ turnDir /\ animation)

  pos' = move maze row col pos moveDir'
