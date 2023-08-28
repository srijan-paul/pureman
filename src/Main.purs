module Main where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now)
import Game.Pureman (drawMaze, loadSpriteSheet, loop, mazeString, newPacman, pacmanDraw, parseMaze)
import Graphics.Canvas (getCanvasElementById, getContext2D, rect, setFillStyle, setStrokeStyle, strokePath)
import Uitl (setImageSmoothing)
import Web.HTML (window)

main :: Effect Unit
main = do
  win <- window
  maybeCanvas <- getCanvasElementById "canvas"

  (Milliseconds time) <- now >>= unInstant >>> pure
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
        let state = { pacman: newPacman atlas, maze }
        drawMaze atlas ctx maze
        loop ctx win state time
