module Game.State
  ( Game
  , State
  , newState
  ) where

import Effect.Ref (Ref)
import Game.Ghost (Ghost, makeBlinky)
import Game.Maze (Maze, pacmanMaze)
import Game.Player.Pacman (Pacman, newPacman)
import Graphics.Canvas (CanvasImageSource)

type State =
  { pacman :: Pacman
  , maze :: Maze
  , ghosts :: Array Ghost
  }

type Game =
  { atlas :: CanvasImageSource
  , stateRef :: Ref State
  }

newState :: CanvasImageSource -> State
newState atlas =
  { pacman: newPacman atlas
  , maze: pacmanMaze
  , ghosts: [ makeBlinky atlas ]
  }

