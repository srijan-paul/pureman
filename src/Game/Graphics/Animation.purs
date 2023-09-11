module Game.Graphics.Animation
  ( Animation
  , Frame
  , drawAnimation
  , mkAnimation
  , stepAnimation
  ) where

import Prelude

import Data.Array (length, (!!))
import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Game.Common (Vec2)
import Graphics.Canvas (CanvasImageSource, Context2D, drawImageFull)

-- A frame is an index into a spritesheet.
type Frame =
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }

type Animation =
  { frames :: Array Frame
  , index :: Int
  , atlas :: CanvasImageSource
  , scale :: Number
  , frameDuration :: Number
  , timeSinceLastFrame :: Number
  }

mkAnimation :: CanvasImageSource -> Array Frame -> Number -> Number -> Animation
mkAnimation atlas frames frameDuration scale =
  { frames
  , atlas
  , frameDuration
  , scale
  , index: 0
  , timeSinceLastFrame: 0.0
  }

stepAnimation :: Number -> Animation -> Animation
stepAnimation dt anim@{ frames, index, timeSinceLastFrame, frameDuration } =
  let
    shouldStep = timeSinceLastFrame + dt > frameDuration
  in
    if shouldStep then anim { timeSinceLastFrame = 0.0, index = stepIndex index }
    else anim { timeSinceLastFrame = timeSinceLastFrame + dt }

  where
  stepIndex idx = if idx + 1 < length frames then idx + 1 else 0

drawAnimation :: Context2D -> Animation -> Vec2 -> Effect Unit
drawAnimation ctx { frames, index, atlas, scale } (x /\ y) = do
  for_ (frames !! index) \frame -> do
    drawImageFull
      ctx
      atlas
      frame.x
      frame.y
      frame.w
      frame.h
      x
      y
      (frame.w * scale)
      (frame.h * scale)
