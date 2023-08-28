module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Game.Pureman (drawMaze, loadSpriteSheet, mazeString, newPacman, pacmanDraw, parseMaze)
import Graphics.Canvas (getCanvasElementById, getContext2D, rect, setFillStyle, setStrokeStyle, strokePath)
import Uitl (setImageSmoothing)

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  for_ maybeCanvas $ \canvas -> do
    ctx <- getContext2D canvas
    setImageSmoothing ctx false
    setStrokeStyle ctx "red"
    setFillStyle ctx "red"
    let path = rect ctx { x: 20.0, y: 20.0, width: 100.0, height: 100.0 }
    strokePath ctx path
    let maze = parseMaze mazeString
    loadSpriteSheet $ \maybeAtlas -> do
      for_ maybeAtlas \atlas -> do
        let pacman = newPacman atlas
        drawMaze atlas ctx maze
        pacmanDraw pacman ctx

