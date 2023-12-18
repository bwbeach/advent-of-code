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
import Debug.Trace (trace, traceShowId)
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
part1 problem = minCost problem (1, 3)

part2 :: Problem -> Int
part2 problem = minCost problem (4, 10)

minCost :: Problem -> (Int, Int) -> Int
minCost problem minMaxStraight =
  minimum . map (traceShowId . costWithInitialDirection problem minMaxStraight) $ [V2 1 0, V2 0 1]

-- | What's the minimum cost starting out in the given direction?
costWithInitialDirection :: Problem -> (Int, Int) -> Point -> Int
costWithInitialDirection problem minMaxStraight@(minStraight, maxStraight) dir0 =
  tracePath problem path cost
  where
    ((w, h), m) = problem
    Just (cost, path) = aStar neighbors transCost estimate found initial
    -- possible next node given a starting node
    neighbors = choices problem minMaxStraight
    -- cost of transitioning from one node to another is the value at the destination
    transCost _ Pos {block = b} = m M.! b
    -- lower boind on the cost to get from a given place to the goal
    estimate = score problem
    -- is a Pos the goal?
    found Pos {block = b, straightCount = s} =
      b == V2 w h && minStraight <= s && s <= maxStraight
    -- the starting place
    initial = Pos {block = V2 1 1, dir = dir0, straightCount = 0}

-- | Print the selected path
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
    -- Returns the new Pos if this move is a valid choice
    maybeChoice (dirFcn, moveType) =
      -- move is valid if we're within the min-max-straight constraint, and stay on the board
      if moveOk && b' `M.member` m
        then Just p0 {block = b', dir = d', straightCount = s'}
        else Nothing
      where
        -- number of times we've gone straight to get to p0
        s = straightCount p0
        -- is this kind of move OK?
        moveOk = case moveType of
          Straight -> s < maxStraight
          Turn -> minStraight <= s
        -- new direction
        d' = dirFcn (dir p0)
        -- new position
        b' = block p0 + d'
        -- new straight count
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
