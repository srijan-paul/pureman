module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Game.Pureman (newGame)
import Graphics.Canvas (getCanvasElementById)
import Web.HTML (window)

main :: Effect Unit
main = do
  win <- window
  maybeCanvas <- getCanvasElementById "canvas"
  for_ maybeCanvas $ \canvas -> do
    newGame win canvas
