module Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (isJust)
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas (getCanvasElementById, getContext2D, rect, setFillStyle, setStrokeStyle, strokePath)

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  for_ maybeCanvas $ \canvas -> do
    ctx <- getContext2D canvas
    setStrokeStyle ctx "red"
    setFillStyle ctx "red"
    let path = rect ctx { x: 20.0, y: 20.0, width: 100.0, height: 100.0 }  
    strokePath ctx path
