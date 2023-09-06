module Game.Maze
  ( Maze
  , Tile
  , TileKind(..)
  , isWall
  , isWallAt
  , mapSize
  , pacmanMaze
  , tileAt
  , tileSize
  ) where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Game.Vec2 (Vec2, vec)
import Data.String as S
import Data.String.CodeUnits (toCharArray)
import Data.Tuple.Nested ((/\), type (/\))

mapSize :: Int /\ Int
mapSize = 31 /\ 32

tileSize :: Number
tileSize = 16.0

--- Representation of the PacMan level 0 maze as a string
--- Taken from the pacman example in https://fable.io/repl/
mazeString ∷ String
mazeString =
  """##/------------7/------------7##
##|............|!............|##
##|./__7./___7.|!./___7./__7.|##
##|o|  !.|   !.|!.|   !.|  !o|##
##|.L--J.L---J.LJ.L---J.L--J.|##
##|..........................|##
##|./__7./7./______7./7./__7.|##
##|.L--J.|!.L--7/--J.|!.L--J.|##
##|......|!....|!....|!......|##
##L____7.|L__7 |! /__J!./____J##
#######!.|/--J LJ L--7!.|#######
#######!.|!          |!.|#######
#######!.|! /__==__7 |!.|#######
/------J.LJ |      ! LJ.L------7
|#######.   | **** !   .#######!
L______7./7 |      ! /7./______J
#######!.|! L______J |!.|#######
#######!.|!          |!.|#######
#######!.|! /______7 |!.|#######
##/----J.LJ L--7/--J LJ.L----7##
##|............|!............|##
##|./__7./___7.|!./___7./__7.|##
##|.L-7!.L---J.LJ.L---J.|/-J.|##
##|o..|!.......<>.......|!..o|##
##L_7.|!./7./______7./7.|!./_J##
##/-J.LJ.|!.L--7/--J.|!.LJ.L-7##
##|......|!....|!....|!......|##
##|./____JL__7.|!./__JL____7.|##
##|.L--------J.LJ.L--------J.|##
##|..........................|##
##L--------------------------J##"""

data TileKind
  = WallNone
  | WallLeft
  | WallRight
  | WallTop
  | WallBottom
  | WallTR
  | WallTL
  | WallBR
  | WallBL

derive instance eqTileKind :: Eq TileKind
type Maze = Array (Array TileKind)

parseMaze :: String -> Maze
parseMaze mazeS =
  map parseTile <<< toCharArray <$> rows
  where
  rows = S.split (S.Pattern "\n") mazeS
  parseTile c
    | c == '-' = WallTop
    | c == '_' = WallBottom
    | c == '/' = WallTL
    | c == '7' = WallTR
    | c == 'L' = WallBL
    | c == 'J' = WallBR
    | c == '|' = WallLeft
    | c == '!' = WallRight
    | otherwise = WallNone

pacmanMaze :: Maze
pacmanMaze = parseMaze mazeString

type Tile =
  { pos :: Vec2
  , size :: Number
  , kind :: TileKind
  }

tileAt :: Maze -> Int -> Int -> Maybe Tile
tileAt maze row col =
  ((maze !! row) >>= \r -> (r !! col)) <#>
    \k ->
      { pos: vec ((toNumber col) * tileSize) ((toNumber row) * tileSize)
      , size: tileSize
      , kind: k
      }

isWall :: TileKind -> Boolean
isWall WallNone = false
isWall _ = true

isWallAt :: Maze -> Int -> Int -> Boolean
isWallAt maze row col = case ((maze !! row) >>= (\r -> r !! col)) of
  Just w -> isWall w
  Nothing -> false
