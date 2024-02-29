module Main where

import Advent (run)
import Algorithm.Search (aStar)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main = run parse part1 part2

type Problem = M.Map String (M.Map String Int)

-- | Parse an iput file
--
-- >>> parse "London to Dublin = 464\n"
-- fromList [("Dublin",fromList [("London",464)]),("London",fromList [("Dublin",464)])]
parse :: String -> Problem
parse =
  foldl' insertTriple M.empty . map parseLine . lines
  where
    insertTriple m (a, b, n) = insertOneWay (a, b, n) . insertOneWay (b, a, n) $ m

    insertOneWay (a, b, n) m =
      M.insert a (M.insert b n (M.findWithDefault M.empty a m)) m

-- | A sample problem for tests
sampleProblem :: Problem
sampleProblem = parse "A to B = 1\nA to C = 2\nB to C = 4\n"

-- | Parse one input line
--
-- >>> parseLine "London to Dublin = 464"
-- ("London","Dublin",464)
parseLine :: String -> (String, String, Int)
parseLine =
  parseWords . words
  where
    parseWords [a, "to", b, "=", n] = (a, b, read n)
    parseWords x = error ("Bad line: " ++ show x)

-- | A search state
data State
  = Start Problem -- initial state; no current location
  | AtCity String Problem -- at a city; Problem is without past cities
  deriving (Eq, Ord, Show)

-- | The problem contained in a state
stateProblem :: State -> Problem
stateProblem (Start p) = p
stateProblem (AtCity _ p) = p

-- | The choices for next state, given a state
choices :: State -> [State]
choices (Start p) =
  map one (M.keys p)
  where
    one a = AtCity a p
choices (AtCity a p) =
  map one (M.keys (p M.! a))
  where
    one b = AtCity b (removeCity a p)

-- | Remove one city from a problem
--
-- Removes the city from both ends of edges.
--
-- >>> removeCity "A" sampleProblem
-- fromList [("B",fromList [("C",4)]),("C",fromList [("B",4)])]
removeCity :: String -> Problem -> Problem
removeCity a = M.map (M.delete a) . M.delete a

part1 :: Problem -> Int
part1 problem =
  case aStar choices cost remaining found (Start problem) of
    Just (c, _) -> c
    Nothing -> error "no solution found"
  where
    cost (Start _) (AtCity _ _) = 0
    cost (AtCity a p) (AtCity b _) = (p M.! a) M.! b
    cost _ _ = error "cannot go to start state"

    remaining :: State -> Int
    remaining s =
      (M.size p - 1) * minDist p
      where
        p = stateProblem s

    minDist :: Problem -> Int
    minDist = minimum . M.map (minimum . (0 :) . M.elems)

    found = (== 1) . M.size . stateProblem

part2 :: Problem -> Int
part2 = length
