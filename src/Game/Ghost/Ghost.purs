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
  , moveDir :: Dir
  , pos :: Vec2
  }

newGhost :: CharacterAnimations -> Vec2 -> Ghost
newGhost animations pos =
  { animations, activeAnimation: animations.right, moveDir: None, pos }

makeBlinky :: CanvasImageSource -> Ghost
makeBlinky atlas =
  newGhost (blinkyAnimations atlas) $ vec (15.0 * tileSize) (15.0 * tileSize)
