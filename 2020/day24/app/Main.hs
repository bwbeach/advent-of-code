module Main where

import Advent (Point, run)
import Data.List (foldl')
import qualified Data.Set as S
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

data Direction
  = E
  | SE
  | SW
  | W
  | NW
  | NE
  deriving (Show)

-- | What colors a tile can show
data Color = White | Black deriving (Eq, Show)

flipTile :: Color -> Color
flipTile White = Black
flipTile Black = White

-- | A vector in hexagon space.
-- The space is mapped two an (x, y) space, with positive x going E
-- and positive y going NW.
type Vector = V2 Int

-- | The path described by one input line
type Path = [Vector]

type Problem = [Path]

parse :: String -> Problem
parse = map parseLine . lines

-- | Convert one input line into a Path
--
-- >>> parseLine "eswnwe"
-- [V2 1 0,V2 (-1) (-1),V2 0 1,V2 1 0]
parseLine :: String -> Path
parseLine =
  map dirToVec . go
  where
    go [] = []
    go ('e' : cs) = E : go cs
    go ('s' : 'e' : cs) = SE : go cs
    go ('s' : 'w' : cs) = SW : go cs
    go ('w' : cs) = W : go cs
    go ('n' : 'w' : cs) = NW : go cs
    go ('n' : 'e' : cs) = NE : go cs
    go s = error ("bad direction: " ++ s)

part1 :: Problem -> Int
part1 = length . blackTiles

blackTiles :: Problem -> [Point]
blackTiles problem =
  S.toList result
  where
    result = foldl' flipOne S.empty locations
    -- Flip one tile by adding/removing it from the set
    flipOne s p = if p `S.member` s then S.delete p s else S.insert p s
    -- The final location for each input line
    locations = map sum problem

part2 :: Problem -> Int
part2 problem =
  S.size . (!! 100) $ steps
  where
    blacks = S.fromList . blackTiles $ problem

    steps = life hexNeighbors isAlive blacks

    isAlive False 2 = True
    isAlive True 1 = True
    isAlive True 2 = True
    isAlive _ _ = False

-- | Convert a Direction into a two-dimensional vector
dirToVec :: Direction -> Point
dirToVec E = V2 2 0
dirToVec SE = V2 1 1
dirToVec SW = V2 (-1) 1
dirToVec W = V2 (-2) 0
dirToVec NW = V2 (-1) (-1)
dirToVec NE = V2 1 (-1)

-- | All six directions
hexDirections :: [Point]
hexDirections = map dirToVec [E, SE, SW, W, NW, NE]

-- | Neighbors of a point
hexNeighbors :: Point -> [Point]
hexNeighbors p = map (p +) hexDirections

-- | Conway's life, generalized
-- Generalizations are:
--   - "neighbors" defines what locations are next to what
--   - "rule" defines when a cell is alive in the next round
--
-- >>> take 4 (life (\n -> [n - 1, n + 1]) (\_ n -> n == 1) (S.singleton (0 :: Int)))
-- [fromList [0],fromList [-1,1],fromList [-2,2],fromList [-3,-1,1,3]]
life ::
  (Ord a) => -- `a` names locations
  (a -> [a]) -> -- neighboring locations
  (Bool -> Int -> Bool) -> -- is a cell alive?  inputs: current state, number of neighbors alive
  S.Set a -> -- initial state is set of cells that are alive
  [S.Set a] -- sequence of states, starting with initial
life neighbors isAlive =
  iterate go
  where
    go prev =
      S.fromList . filter check . candidates $ prev
      where
        -- is this location alive in the next round
        check a = isAlive (a `S.member` prev) (numNeighborsAlive a)
        -- how many neighbors were alive in the previous round
        numNeighborsAlive = length . filter (`S.member` prev) . neighbors
    -- locations that *might* be alive in next round
    candidates = unique . concatMap selfAndNeighbors . S.toList
    -- a location and all of its neighbors
    selfAndNeighbors a = a : neighbors a
    -- unique values from a list
    unique = S.toList . S.fromList

-- | Convert a set of hexy points to a pretty display
-- showHex :: S.Set Point -> String
-- showHex points =
--   unlines . map oneLine $ [(y0 - 1) .. (y1 + 1)]
--   where
--     oneLine y = map (oneChar y) [(x0 - 1) .. (x1 + 1)]
--     oneChar y x
--       | V2 x y `S.member` points = '*'
--       | x == 0 && y == 0 = 'X'
--       | x == y = '\\'
--       | x == -y = '/'
--       | y == 0 && even x = '-'
--       | even (x + y) = 'o'
--       | otherwise = ' '
--     (V2 x0 y0, V2 x1 y1) = pointBounds . S.toList $ points

-- | Bounds for a set of points
--
-- >>> pointBounds [V2 3 4, V2 1 2]
-- (V2 1 2,V2 3 4)
pointBounds :: [Point] -> (Point, Point)
pointBounds ps =
  (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  where
    xs = map (\(V2 x _) -> x) ps
    ys = map (\(V2 _ y) -> y) ps
