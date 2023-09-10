module Game.Graphics.Sprite
  ( CharacterAnimations
  , blinkyAnimations
  , dirToAnimation
  , loadSpriteSheet
  , makePacmanAnimations
  , pinkyAnimations
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Game.Common (Dir(..))
import Game.Graphics.Animation (Animation, mkAnimation)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)

loadSpriteSheet :: (Maybe CanvasImageSource -> Effect Unit) -> Effect Unit
loadSpriteSheet = tryLoadImage "spritesheet.png"

type CharacterAnimations =
  { left :: Animation
  , right :: Animation
  , up :: Animation
  , down :: Animation
  }

ghostFrameDuration :: Number
ghostFrameDuration = 120.0

makePacmanAnimations :: CanvasImageSource -> CharacterAnimations
makePacmanAnimations atlas =
  { left: mkAnimation atlas leftFrames 100.0 2.0
  , right: mkAnimation atlas rightFrames 100.0 2.0
  , up: mkAnimation atlas upFrames 100.0 2.0
  , down: mkAnimation atlas downFrames 100.0 2.0
  }
  where
  rightFrames =
    [ { x: 456.0, y: 0.0, w: 16.0, h: 16.0 }
    , { x: 472.0, y: 0.0, w: 16.0, h: 16.0 }
    , { x: 488.0, y: 0.0, w: 16.0, h: 16.0 }
    ]

  leftFrames =
    [ { x: 457.0, y: 16.0, w: 16.0, h: 16.0 }
    , { x: 472.0, y: 16.0, w: 16.0, h: 16.0 }
    , { x: 488.0, y: 0.0, w: 16.0, h: 16.0 }
    ]

  upFrames =
    [ { x: 455.0, y: 32.0, w: 16.0, h: 16.0 }
    , { x: 471.0, y: 32.0, w: 16.0, h: 16.0 }
    , { x: 488.0, y: 0.0, w: 16.0, h: 16.0 }
    ]
  downFrames =
    [ { x: 455.0, y: 48.0, w: 16.0, h: 16.0 }
    , { x: 471.0, y: 48.0, w: 16.0, h: 16.0 }
    , { x: 488.0, y: 0.0, w: 16.0, h: 16.0 }
    ]

blinkyAnimations :: CanvasImageSource -> CharacterAnimations
blinkyAnimations atlas =
  { left: mkAnimation atlas leftFrames ghostFrameDuration 2.0
  , right: mkAnimation atlas rightFrames ghostFrameDuration 2.0
  , up: mkAnimation atlas upFrames ghostFrameDuration 2.0
  , down: mkAnimation atlas downFrames ghostFrameDuration 2.0
  }
  where
  rightFrames =
    [ { x: 456.0, y: 64.0, w: 16.0, h: 16.0 }
    , { x: 472.0, y: 64.0, w: 16.0, h: 16.0 }
    ]

  leftFrames =
    [ { x: 488.0, y: 64.0, w: 16.0, h: 16.0 }
    , { x: 504.0, y: 64.0, w: 16.0, h: 16.0 }
    ]

  upFrames =
    [ { x: 520.0, y: 64.0, w: 16.0, h: 16.0 }
    , { x: 536.0, y: 64.0, w: 16.0, h: 16.0 }
    ]
  downFrames =
    [ { x: 552.0, y: 64.0, w: 16.0, h: 16.0 }
    , { x: 568.0, y: 64.0, w: 16.0, h: 16.0 }
    ]

pinkyAnimations :: CanvasImageSource -> CharacterAnimations
pinkyAnimations atlas =
  { left: mkAnimation atlas leftFrames frameDuration 2.0
  , right: mkAnimation atlas rightFrames ghostFrameDuration 2.0
  , up: mkAnimation atlas upFrames ghostFrameDuration 2.0
  , down: mkAnimation atlas downFrames ghostFrameDuration 2.0
  }
  where
  frameDuration = 200.0

  rightFrames =
    [ { x: 456.0, y: 80.0, w: 16.0, h: 16.0 }
    , { x: 472.0, y: 80.0, w: 16.0, h: 16.0 }
    ]

  leftFrames =
    [ { x: 488.0, y: 80.0, w: 16.0, h: 16.0 }
    , { x: 504.0, y: 80.0, w: 16.0, h: 16.0 }
    ]

  upFrames =
    [ { x: 520.0, y: 80.0, w: 16.0, h: 16.0 }
    , { x: 536.0, y: 80.0, w: 16.0, h: 16.0 }
    ]
  downFrames =
    [ { x: 552.0, y: 80.0, w: 16.0, h: 16.0 }
    , { x: 568.0, y: 80.0, w: 16.0, h: 16.0 }
    ]

dirToAnimation :: CharacterAnimations -> Dir -> Animation
dirToAnimation anims dir = case dir of
  Left -> anims.left
  Right -> anims.right
  Up -> anims.up
  Down -> anims.down
  _ -> anims.right

