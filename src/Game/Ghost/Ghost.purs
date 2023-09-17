module Game.Ghost
  ( ControlScheme
  , Ghost
  , GhostBehavior
  , GhostMode(..)
  , GhostModeScheme
  , currentMode
  , makeBlinky
  , makePinky
  ) where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Game.Common (Dir(..), tileSize, Vec2, vec)
import Game.Graphics.Animation (Animation)
import Game.Graphics.Sprite (CharacterAnimations, blinkyAnimations, pinkyAnimations)
import Game.Maze (mapSize, toRowCol)
import Game.Player.Pacman (Pacman)
import Graphics.Canvas (CanvasImageSource)

-- Given pacman's current location, return a target tile.

data GhostMode = Scatter | Chase

derive instance ghostModeEq :: Eq GhostMode

type GhostModeScheme = Array (GhostMode /\ Number)
type GhostBehavior =
  { timeSpentInCurrentMode :: Number
  , scheme :: GhostModeScheme
  , currentModeIndex :: Int
  }

type ControlScheme = GhostMode -> Pacman -> (Int /\ Int)

type Ghost =
  { animations :: CharacterAnimations
  , activeAnimation :: Animation
  , moveDir :: Dir -- direction in which the ghost is currently moving.
  , turnDir :: Dir -- direction in which the ghost wants to turn.
  , pos :: Vec2
  , ai :: ControlScheme
  , behavior :: GhostBehavior
  }

newBehavor :: GhostModeScheme -> GhostBehavior
newBehavor scheme =
  { timeSpentInCurrentMode: 0.0
  , scheme
  , currentModeIndex: 0
  }

currentMode :: Ghost -> GhostMode
currentMode { behavior: { scheme, currentModeIndex } } =
  case scheme !! currentModeIndex of
    Just (mode /\ _) -> mode
    Nothing -> Chase

newGhost :: CharacterAnimations -> Vec2 -> ControlScheme -> GhostModeScheme -> Ghost
newGhost animations pos ai scheme =
  { animations
  , activeAnimation: animations.right
  , moveDir: None
  , turnDir: None
  , pos
  , ai
  , behavior: newBehavor scheme
  }

ghostModeScheme ∷ GhostModeScheme
ghostModeScheme =
  [ (Scatter /\ 5000.0)
  , (Chase /\ 7000.0)
  ]

makeBlinky :: CanvasImageSource -> Ghost
makeBlinky atlas =
  newGhost
    (blinkyAnimations atlas)
    (vec (15.0 * tileSize) (15.0 * tileSize))
    ai
    ghostModeScheme
  where
  ai mode pacman =
    if mode == Chase then
      toRowCol pacman.pos
    else
      (3 /\ 0)

makePinky :: CanvasImageSource -> Ghost
makePinky atlas =
  newGhost
    (pinkyAnimations atlas)
    (vec (16.0 * tileSize) (15.0 * tileSize))
    ai
    ghostModeScheme
  where
  -- pinky always targets 4 tiles ahead of pacman in
  -- his direction of movement.
  ai mode pacman =
    if mode == Chase then
      let
        (pacRow /\ pacCol) = toRowCol pacman.pos
      in
        case pacman.moveDir of
          Up -> (pacRow - 4) /\ pacCol
          Down -> (pacRow + 4) /\ pacCol
          Left -> pacRow /\ (pacCol - 4)
          Right -> pacRow /\ (pacCol + 4)
          None -> (pacRow /\ pacCol)
    else
      (0 /\ snd mapSize)
