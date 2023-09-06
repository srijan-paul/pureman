module Game.Ghost.Update where

import Data.Int
import Prelude

import Data.Array (filter, sortBy, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Debug.Debug as Debug
import Effect (Effect)
import Game.Common (Dir, collision, dir2Vector, oppositeDir, pair2Dir, tileSize)
import Game.Ghost (Ghost)
import Game.Graphics.Animation (drawAnimation, stepAnimation)
import Game.Graphics.Sprite (dirToAnimation)
import Game.Maze (Maze, at, isWall, tileAt, toRowCol, turnRowCol)
import Game.State (State)
import Game.Vec2 (Vec2)
import Graphics.Canvas (Context2D)

type TilePosition = Tuple Int Int

tileDist :: Int /\ Int -> Int /\ Int -> Int
tileDist (r1 /\ c1) (r2 /\ c2) =
  dr * dr + dc * dc
  where
  dr = r2 - r1
  dc = c2 - c1

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

updateGhost :: Number -> State -> Ghost -> Ghost
updateGhost dt { maze, pacman } ghost@{ animations, moveDir } =
  ghost
    { activeAnimation = stepAnimation dt activeAnimation'
    , moveDir = moveDir'
    , pos = move maze row col ghost.pos moveDir'
    }

  where
  pacmanPos = toRowCol pacman.pos
  (row /\ col) = toRowCol ghost.pos

  -- list of tile coordinates that ghost can move to
  candidateTiles =
    filter
      ( \(r /\ c) -> case at maze r c of
          -- A ghost can only move to a tile if it is empty
          Just w -> not $ isWall w
          -- If out of bounds, then this tile cannot be considered for movement 
          Nothing -> false
      )
      [ ((row - 1) /\ col), ((row + 1) /\ col), (row /\ (col + 1)), (row /\ (col - 1)) ]

  sortedTiles =
    let
      cmp a b = compare (tileDist pacmanPos a) (tileDist pacmanPos b)
    in
      sortBy cmp candidateTiles

  possibleDirs =
    filter
      ((/=) $ oppositeDir moveDir) $
      (pair2Dir <<< (flip (-) (row /\ col))) <$> sortedTiles

  moveDir' = case possibleDirs !! 0 of
    Just dir -> dir
    Nothing -> moveDir
  activeAnimation' = dirToAnimation animations moveDir'

drawGhost :: Context2D -> Ghost -> Effect Unit
drawGhost ctx { pos, activeAnimation } =
  drawAnimation
    ctx
    activeAnimation
    (pos - ((tileSize / 2.0) /\ (tileSize / 2.0)))