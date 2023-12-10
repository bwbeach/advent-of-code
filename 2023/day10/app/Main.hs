module Main where

import Advent
  ( Grid (..),
    Point,
    gridBounds,
    gridGet,
    gridMap,
    gridParse,
    only,
    run,
  )
import Data.Bits (Bits (xor))
import Data.Function ((&))
import Data.List (foldl', sort)
import qualified Data.Map as M
import qualified Data.Set as S
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse . replace '.' ' '

part1 :: Problem -> Int
part1 problem = length (findPath problem) `div` 2

part2 :: Problem -> Int
part2 problem =
  problem
    -- Find the points that comprise the path
    & findPath
    -- Translate that to a list of rows, each of which is like ".|L-7.F-J|."
    & pathToRows problem
    -- Calculate the number of "inside" points in each row
    & map countInside
    -- Add up the results
    & sum

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)

pipeConnections :: Char -> [Point]
pipeConnections '|' = [north, south]
pipeConnections '-' = [east, west]
pipeConnections 'L' = [north, east]
pipeConnections 'J' = [north, west]
pipeConnections '7' = [south, west]
pipeConnections 'F' = [south, east]
pipeConnections 'S' = [north, east, south, west]
pipeConnections ' ' = []

west :: Point
west = V2 (-1) 0

north :: Point
north = V2 0 (-1)

south :: Point
south = V2 0 1

east :: Point
east = V2 1 0

-- | What is the starting point?
start :: Problem -> Point
start = fst . only . filter ((== 'S') . snd) . M.toList . gridMap

-- | What neighboring points does this one connect to?
-- Checks that the connection works in both directions, which
-- lets it work for "S", which can connect anywhere, but only
-- if the neighboring piece connects back.
connectsTo :: Problem -> Point -> [Point]
connectsTo problem start =
  filter (connectsBackTo start) . goFrom $ start
  where
    -- Based on just the pipe piece at p, where can you go?
    goFrom p = map (+ p) (pipeConnections (getPipe p))
    -- Does the piece at `x` connect to the piece at `p`?
    connectsBackTo p x = p `elem` goFrom x
    -- What pipe piece is at `p`?
    getPipe p = gridGet p problem

-- | Find the path from the start point back to itself.
findPath :: Problem -> [Point]
findPath problem =
  s : go s s1 s
  where
    -- the start point
    s = start problem
    -- pick where to go from the start point; either way works
    s1 = head (connectsTo problem s)
    -- find the rest of the path from `back0`, not backtracking to `back1`
    go back1 back0 dest =
      if back0 == dest
        then [back0]
        else back0 : go back0 (next back1 back0) dest
    -- where can you go from `back0` that isn't `back1`?
    next back1 back0 = only $ filter (/= back1) (connectsTo problem back0)

-- | Make the subset of the grid that is just the path, and slice it into rows.
pathToRows :: Problem -> [Point] -> [String]
pathToRows problem path =
  [oneRow y | y <- [y0 .. y1]]
  where
    oneRow y = [gridGet (V2 x y) justPath | x <- [x0 .. x1]]
    (V2 x0 y0, V2 x1 y1) = gridBounds problem
    pathSet = S.fromList path
    justPath = Grid pathMap'
    pathMap = M.fromList . filter ((`S.member` pathSet) . fst) . M.toList . gridMap $ problem
    s = start problem
    sPiece = startPiece problem
    pathMap' = M.insert s sPiece pathMap

-- | What kind of piece goes at the starting point?
startPiece :: Problem -> Char
startPiece problem =
  connectionsToPiece (sort deltas)
  where
    s = start problem
    deltas :: [Point]
    deltas = map (\x -> x - s) connections
    connections = connectsTo problem s

-- | Given the connections of a start piece, what is it?
connectionsToPiece :: [V2 Int] -> Char
connectionsToPiece x
  | x == [west, south] = '7'
  | x == [west, east] = '-'
  | x == [north, south] = '|'
  | x == [north, east] = 'L'
  | x == [south, east] = 'F'

-- | Count the number of inside cells in a row
--
-- Uses a little state machine with these states:
--   'O' - outside the pipe
--   'I' - inside the pipe
--   'L' - crossed an L piece from outside, or an F piece from inside
--   'F' - crossed an F piece from outside, or an L piece from inside
countInside =
  fst . foldl' next (0, 'O')
  where
    next (n, 'O') ' ' = (n, 'O')
    next (n, 'O') '|' = (n, 'I')
    next (n, 'O') 'F' = (n, 'F')
    next (n, 'O') 'L' = (n, 'L')
    next (n, 'F') '-' = (n, 'F')
    next (n, 'F') 'J' = (n, 'I')
    next (n, 'F') '7' = (n, 'O')
    next (n, 'L') '-' = (n, 'L')
    next (n, 'L') 'J' = (n, 'O')
    next (n, 'L') '7' = (n, 'I')
    next (n, 'I') ' ' = (n + 1, 'I')
    next (n, 'I') '|' = (n, 'O')
    next (n, 'I') 'L' = (n, 'F')
    next (n, 'I') 'F' = (n, 'L')
    next s c = error ("No transition for " ++ show s ++ " " ++ show c)
