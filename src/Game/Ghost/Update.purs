module Game.Ghost.Update where

import Prelude

import Data.Array (filter, length, sortBy, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Game.Common (oppositeDir, pair2Dir, tileSize)
import Game.Entity (canTurn, move)
import Game.Ghost (Ghost, GhostBehavior, currentMode)
import Game.Graphics.Animation (drawAnimation, stepAnimation)
import Game.Graphics.Sprite (dirToAnimation)
import Game.Maze (at, isWall, toRowCol)
import Game.State (State)
import Graphics.Canvas (Context2D)

--- Compute distance between two tiles.
tileDist :: Int /\ Int -> Int /\ Int -> Int
tileDist (r1 /\ c1) (r2 /\ c2) =
  dr * dr + dc * dc
  where
  dr = r2 - r1
  dc = c2 - c1

updateBehavior :: Number -> GhostBehavior -> GhostBehavior
updateBehavior
  dt
  behavior@{ timeSpentInCurrentMode, scheme, currentModeIndex } =
  case scheme !! currentModeIndex of
    Just (_ /\ modeDuration) ->
      let
        timeSpent' =
          if timeSpentInCurrentMode + dt > modeDuration then 0.0
          else timeSpentInCurrentMode + dt
        nextSchemeIndex =
          if currentModeIndex + 1 < length scheme then currentModeIndex + 1
          else 0
        schemeIndex' =
          if timeSpent' == 0.0 then nextSchemeIndex
          else currentModeIndex
      in
        behavior { timeSpentInCurrentMode = timeSpent', currentModeIndex = schemeIndex' }
    Nothing -> behavior

updateGhost :: Number -> State -> Ghost -> Ghost
updateGhost dt { maze, pacman } ghost@{ pos, animations, moveDir, activeAnimation, ai, behavior } =
  ghost
    { activeAnimation = stepAnimation dt activeAnimation'
    , pos = move maze row col ghost.pos moveDir'
    , moveDir = moveDir'
    , behavior = updateBehavior dt behavior
    }

  where
  mode = currentMode ghost
  targetTile = ai mode pacman
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
      cmp a b = compare (tileDist targetTile a) (tileDist targetTile b)
    in
      sortBy cmp candidateTiles

  possibleDirs =
    filter
      ((/=) $ oppositeDir moveDir) $
      (pair2Dir <<< (flip (-) (row /\ col))) <$> sortedTiles

  (moveDir' /\ activeAnimation') = case possibleDirs !! 0 of
    Just dir ->
      if canTurn maze row col pos dir then (dir /\ dirToAnimation animations dir)
      else (moveDir /\ activeAnimation)
    Nothing -> (moveDir /\ activeAnimation)

drawGhost :: Context2D -> Ghost -> Effect Unit
drawGhost ctx { pos, activeAnimation } =
  drawAnimation
    ctx
    activeAnimation
    (pos - ((tileSize / 2.0) /\ (tileSize / 2.0)))
