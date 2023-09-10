module Game.Ghost
  ( Ghost
  , makeBlinky
  ) where

import Prelude

import Game.Graphics.Animation (Animation)
import Game.Common (Dir(..), tileSize)
import Game.Graphics.Sprite (CharacterAnimations, blinkyAnimations)
import Game.Vec2 (Vec2, vec)
import Graphics.Canvas (CanvasImageSource)

type Ghost =
  { animations :: CharacterAnimations
  , activeAnimation :: Animation
  , moveDir :: Dir -- direction in which the ghost is currently moving.
  , turnDir :: Dir -- direction in which the ghost wants to turn.
  , pos :: Vec2
  }

newGhost :: CharacterAnimations -> Vec2 -> Ghost
newGhost animations pos =
  { animations, activeAnimation: animations.right, moveDir: None, turnDir: None, pos }

makeBlinky :: CanvasImageSource -> Ghost
makeBlinky atlas =
  newGhost (blinkyAnimations atlas) $ vec (15.0 * tileSize) (15.0 * tileSize)
