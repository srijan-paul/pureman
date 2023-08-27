module Uitl where

import Prelude

import Effect (Effect)
import Graphics.Canvas (Context2D)

foreign import setImageSmoothing :: Context2D -> Boolean -> Effect Unit
