module Game.State
  ( Game
  , State
  , newState
  ) where

import Effect.Ref (Ref)
import Game.Food (Food, allFoods)
import Game.Ghost (Ghost, makeBlinky, makePinky)
import Game.Maze (Maze, pacmanMaze)
import Game.Player.Pacman (Pacman, newPacman)
import Graphics.Canvas (CanvasImageSource)

type State =
  { pacman :: Pacman
  , maze :: Maze
  , ghosts :: Array Ghost
  , foods :: Array Food
  , score :: Int
  }

type Game =
  { atlas :: CanvasImageSource
  , stateRef :: Ref State
  }

newState :: CanvasImageSource -> State
newState atlas =
  let
    maze = pacmanMaze
  in
    { pacman: newPacman atlas
    , maze
    , ghosts: [ makeBlinky atlas, makePinky atlas ]
    , foods: allFoods
    , score: 0
    }
