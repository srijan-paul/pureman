module Game.Entity
  ( canTurn
  , move
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Game.Common (Dir, Vec2, aligned, collision, dir2Vector)
import Game.Maze (Maze, isWall, isWallAt, tileAt, turnRowCol)

move :: Maze -> Int -> Int -> Vec2 -> Dir -> Vec2
move maze row col pos dir =
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

canTurn :: Maze -> Int -> Int -> Vec2 -> Dir -> Boolean
canTurn maze row col pos turnDir =
  -- ghost cannot turn unless aligned to a grid cell
  if not $ aligned (fst pos) && aligned (snd pos) then false
  else
    let
      -- ghost cannot turn if the tile in turn direction is a wall
      nextRow /\ nextCol = turnRowCol row col turnDir
    in
      not $ isWallAt maze nextRow nextCol
