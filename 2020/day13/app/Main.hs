module Main where

import Advent (run)
import Data.Foldable (Foldable (foldl'))
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

part2 :: Problem -> Integer
part2 (_, buses) =
  fst . foldl' merge (0, 1) . mapMaybe justBusId . zip [0 ..] $ buses
  where
    merge (t0, p0) (t1, p1) =
      (nextMatch, lcm p0 p1)
      where
        nextMatch = head . filter isMatch . iterate (+ p0) $ t0
        isMatch t = (t - t1) `mod` p1 == 0

    justBusId (i, X) = Nothing
    justBusId (i, Id bid) = Just (toInteger (bid - i), toInteger bid)
