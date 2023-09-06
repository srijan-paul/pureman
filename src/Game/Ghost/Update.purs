module Game.Ghost.Update where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Game.Common (tileSize)
import Game.Ghost (Ghost)
import Game.Graphics.Animation (drawAnimation, stepAnimation)
import Game.State (State)
import Graphics.Canvas (Context2D)

updateGhost :: Number -> State -> Ghost -> Ghost
updateGhost dt _ ghost@{ activeAnimation } =
  ghost { activeAnimation = stepAnimation dt activeAnimation }

drawGhost :: Context2D -> Ghost -> Effect Unit
drawGhost ctx { pos, activeAnimation } =
  drawAnimation
    ctx
    activeAnimation
    (pos - ((tileSize / 2.0) /\ (tileSize / 2.0)))