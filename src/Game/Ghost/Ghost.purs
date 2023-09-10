module Game.Ghost
  ( ControlScheme
  , Ghost
  , makeBlinky
  , makePinky
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Game.Common (Dir(..), tileSize)
import Game.Graphics.Animation (Animation)
import Game.Graphics.Sprite (CharacterAnimations, blinkyAnimations, pinkyAnimations)
import Game.Maze (toRowCol)
import Game.Player.Pacman (Pacman)
import Game.Vec2 (Vec2, vec)
import Graphics.Canvas (CanvasImageSource)
import Undefined (undefined)

-- Given pacman's current location, return a target tile.
type ControlScheme = Pacman -> (Int /\ Int)

type Ghost =
  { animations :: CharacterAnimations
  , activeAnimation :: Animation
  , moveDir :: Dir -- direction in which the ghost is currently moving.
  , turnDir :: Dir -- direction in which the ghost wants to turn.
  , pos :: Vec2
  , ai :: ControlScheme
  }

newGhost :: CharacterAnimations -> Vec2 -> ControlScheme -> Ghost
newGhost animations pos ai =
  { animations
  , activeAnimation: animations.right
  , moveDir: None
  , turnDir: None
  , pos
  , ai
  }

makeBlinky :: CanvasImageSource -> Ghost
makeBlinky atlas =
  newGhost
    (blinkyAnimations atlas)
    (vec (15.0 * tileSize) (15.0 * tileSize))
    ai
  where
  ai pacman = toRowCol pacman.pos

makePinky :: CanvasImageSource -> Ghost
makePinky atlas =
  newGhost
    (pinkyAnimations atlas)
    (vec (16.0 * tileSize) (15.0 * tileSize))
    ai
  where
  -- pinky always targets 4 tiles ahead of pacman in
  -- his direction of movement.
  ai pacman =
    let
      (pacRow /\ pacCol) = toRowCol pacman.pos
    in
      case pacman.moveDir of
        Up -> (pacRow - 4) /\ pacCol
        Down -> (pacRow + 4) /\ pacCol
        Left -> pacRow /\ (pacCol - 4)
        Right -> pacRow /\ (pacCol + 4)
        None -> (pacRow /\ pacCol)
