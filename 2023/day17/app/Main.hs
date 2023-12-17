module Main where

import Advent
  ( Grid (..),
    Point,
    gridBounds,
    gridFormat,
    gridMap,
    gridParse,
    run,
  )
import Algorithm.Search (aStar)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Tuple.Extra (second)
import Debug.Trace
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem =
  ( (Int, Int), -- width and height of the city
    M.Map Point Int -- map from position of city block to heat loss amount (cost)
  )

parse :: String -> Problem
parse text =
  ((w, h), m)
  where
    grid = gridParse text
    (_, V2 w h) = gridBounds grid
    m = M.fromList . map (second readChar) . M.toList . gridMap $ grid
    readChar c = read [c]

part1 :: Problem -> Int
part1 problem =
  costWithInitialDirection problem (V2 1 0)

costWithInitialDirection :: Problem -> Point -> Int
costWithInitialDirection problem dir0 =
  tracePath problem path cost
  where
    ((w, h), m) = problem
    Just (cost, path) = aStar neighbors transCost estimate found initial
    neighbors = choices problem
    transCost _ Pos {block = b} = m M.! b
    estimate = score problem
    found Pos {block = b} = b == V2 w h
    initial = Pos {block = V2 1 1, dir = dir0, straightCount = 0}

part2 :: Problem -> Int
part2 = length

tracePath :: Problem -> [Pos] -> a -> a
tracePath (_, m) path =
  trace formattedPath
  where
    formattedPath = gridFormat grid ++ "\n\n"
    grid = Grid (M.fromList $ problemElems ++ pathElems)
    problemElems = map (second (head . show)) . M.toList $ m
    pathElems = map pathElem path
    pathElem Pos {block = b, dir = d} = (b, dirChar d)

    dirChar (V2 1 0) = '>'
    dirChar (V2 (-1) 0) = '<'
    dirChar (V2 0 1) = 'v'
    dirChar (V2 0 (-1)) = '^'

-- | The state that says where we are and where we can go.
data Pos = Pos
  { block :: Point, -- the position of the current city block in the grid
    dir :: Point, -- the direction we moved to get here
    straightCount :: Int -- how many times in a row we moved in this direction
  }
  deriving (Eq, Ord, Show)

-- | The places you can move next from a given position.
choices :: Problem -> Pos -> [Pos]
choices ((w, h), m) p0 =
  catMaybes [left, right, straight]
  where
    dir0 = dir p0
    block0 = block p0
    leftDir = turnLeft dir0
    leftBlock = block0 + leftDir
    left =
      case M.lookup leftBlock m of
        Nothing -> Nothing
        Just _ -> Just p0 {block = leftBlock, dir = leftDir, straightCount = 1}
    rightDir = turnRight dir0
    rightBlock = block0 + rightDir
    right =
      case M.lookup rightBlock m of
        Nothing -> Nothing
        Just _ -> Just p0 {block = rightBlock, dir = rightDir, straightCount = 1}
    straightDir = dir0
    straightBlock = block0 + straightDir
    straight =
      if straightCount p0 == 3
        then Nothing
        else case M.lookup straightBlock m of
          Nothing -> Nothing
          Just _ -> Just p0 {block = straightBlock, dir = dir0, straightCount = straightCount p0 + 1}

-- | Score a Pos for A-star search.
-- The score must not exceed the actual score.  Higher is better for search.
-- This function uses the manhattan distance from the current location to the goal,
-- which assumes that the cost at each block is at least 1.
score :: Problem -> Pos -> Int
score ((w, h), _) Pos {block = V2 x y} = abs (w - x) + abs (h - y)

turnLeft :: (Num a) => V2 a -> V2 a
turnLeft (V2 x y) = V2 y (-x)

turnRight :: (Num a) => V2 a -> V2 a
turnRight (V2 x y) = V2 (-y) x
