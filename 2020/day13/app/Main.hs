module Main where

import Advent (run)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

main :: IO ()
main = run parse part1 part2

data BusId
  = X
  | Id Int
  deriving (Eq, Show)

type Problem = (Int, [BusId])

parse :: String -> Problem
parse text =
  (earliest, buses)
  where
    [line1, line2] = lines text
    earliest = read line1
    buses = map parseBus . splitOn "," $ line2
    parseBus "x" = X
    parseBus ns = Id (read ns)

part1 :: Problem -> Int
part1 (earliest, buses) =
  (t - earliest) * i
  where
    (t, i) = minimum . mapMaybe arrival $ buses
    arrival X = Nothing
    arrival (Id bid) = Just ((earliest `div` bid) * bid + bid, bid)

part2 :: Problem -> Int
part2 = length
