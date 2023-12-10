module Main where

import Advent
  ( Grid (..),
    Point,
    gridGet,
    gridMap,
    gridParse,
    only,
    run,
  )
import qualified Data.Map as M
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse . replace '.' ' '

part1 :: Problem -> Int
part1 problem = length (findPath problem) `div` 2

part2 :: Problem -> Int
part2 = M.size . gridMap

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

north :: Point
north = V2 0 (-1)

south :: Point
south = V2 0 1

west :: Point
west = V2 (-1) 0

east :: Point
east = V2 1 0

-- | What is the starting point?
start :: Problem -> Point
start = fst . only . filter ((== 'S') . snd) . M.toList . gridMap

-- | What neighboring points does this one connect to?
connectsTo :: Problem -> Point -> [Point]
connectsTo problem start =
  filter (connectsBackTo start) . goFrom $ start
  where
    connectsBackTo p x = p `elem` goFrom x
    goFrom p = map (+ p) (pipeConnections (getPipe p))
    getPipe p = gridGet p problem

-- | Find the path from the start point back to itself.
findPath :: Problem -> [Point]
findPath problem =
  s : go s s1 s
  where
    s = start problem
    s1 = head (connectsTo problem s)
    go :: Point -> Point -> Point -> [Point]
    go back1 back0 dest =
      if back0 == dest
        then [back0]
        else back0 : go back0 (next back1 back0) dest
    next back1 back0 = only $ filter (/= back1) (connectsTo problem back0)