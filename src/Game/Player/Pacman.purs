module Game.Player.Pacman
  ( Pacman
  , newPacman
  ) where

import Prelude

import Game.Graphics.Animation (Animation)
import Game.Common (Dir(..), tileSize)
import Game.Graphics.Sprite (CharacterAnimations, makePacmanAnimations)
import Game.Vec2 (Vec2, vec)
import Graphics.Canvas (CanvasImageSource)

type Pacman =
  { animation :: Animation
  -- current movement direction
  , moveDir :: Dir
  -- direction in which the player wants pacman to move next
  , turnDir :: Dir
  , pos :: Vec2
  , size :: Number -- hitbox size in pixels
  , animations :: CharacterAnimations
  }

newPacman :: CanvasImageSource -> Pacman
newPacman atlas =
  { animations
  , pos: vec (tileSize * 16.0) (tileSize * 23.0)
  , size: tileSize
  , moveDir: None
  , turnDir: None
  , animation: animations.right
  }

  where
  animations = makePacmanAnimations atlas

