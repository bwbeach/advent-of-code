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
import Data.Maybe (mapMaybe)
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
  minimum . map (traceShowId . costWithInitialDirection problem (1, 3)) $ [V2 1 0, V2 0 1]

costWithInitialDirection :: Problem -> (Int, Int) -> Point -> Int
costWithInitialDirection problem turnLimits dir0 =
  tracePath problem path cost
  where
    ((w, h), m) = problem
    Just (cost, path) = aStar neighbors transCost estimate found initial
    neighbors = choices problem turnLimits
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

-- | Types of moves
data MoveType = Straight | Turn deriving (Show)

-- | The places you can move next from a given position.
choices :: Problem -> (Int, Int) -> Pos -> [Pos]
choices ((w, h), m) (minStraight, maxStraight) p0 =
  mapMaybe maybeChoice [(turnLeft, Turn), (turnRight, Turn), (id, Straight)]
  where
    maybeChoice (dirFcn, moveType) =
      if b' `M.member` m && moveOk
        then Just p0 {block = b', dir = d', straightCount = s'}
        else Nothing
      where
        s = straightCount p0
        moveOk = case moveType of
          Straight -> s < maxStraight
          Turn -> minStraight <= s
        d' = dirFcn (dir p0)
        b' = block p0 + d'
        s' = case moveType of
          Straight -> 1 + s
          Turn -> 1

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
