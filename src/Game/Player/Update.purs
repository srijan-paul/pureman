module Game.Player.Update
  ( pacmanDraw
  , pacmanUpdate
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Game.Common (Dir(..), collision, tileSize, aligned, dir2Vector)
import Game.Graphics.Animation (drawAnimation, stepAnimation)
import Game.Graphics.Sprite (dirToAnimation)
import Game.Maze (isWall, isWallAt, tileAt, toRowCol, turnRowCol)
import Game.Player.Pacman (Pacman)
import Game.State (State)
import Game.Vec2 (Vec2)
import Graphics.Canvas (Context2D)

pacmanDraw :: Context2D -> Pacman -> Effect Unit
pacmanDraw ctx { animation, pos } = do
  drawAnimation ctx animation (pos - ((tileSize / 2.0) /\ (tileSize / 2.0)))

pacmanUpdate :: Number -> State -> Pacman -> Pacman
pacmanUpdate dt { maze } pureman@{ animation, pos, moveDir, turnDir } =
  pureman
    { animation = stepAnimation dt animation'
    , pos = move moveDir'
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

  (moveDir' /\ turnDir' /\ animation') =
    if turnDir /= None && canTurn then
      (turnDir /\ None /\ (dirToAnimation pureman.animations turnDir))
    else
      (moveDir /\ turnDir /\ animation)

  move :: Dir -> Vec2
  move dir =
    let
      dpos = dir2Vector dir
      newPos = pos + dpos
      (row' /\ col') = turnRowCol row col dir
    in
      case tileAt maze row' col' of
        Just tile ->
          if (isWall tile.kind && collision newPos tile.pos) then
            pos
          else
            newPos
        Nothing -> pos