module Main where

import Advent (run)
import Data.List.Extra (dropEnd, find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)

main :: IO ()
main = run parse part1 part2

data RangeMapping = RangeMapping
  { destStart :: Int,
    srcStart :: Int,
    rangeLen :: Int
  }
  deriving (Eq, Show)

-- | Use a range mapping to map one number, if it's in range.
--
-- >>> rangeMap (RangeMapping {destStart = 4, srcStart = 14, rangeLen = 3}) 15
-- Just 5
--
-- >>> rangeMap (RangeMapping {destStart = 4, srcStart = 14, rangeLen = 3}) 17
-- Nothing
rangeMap :: RangeMapping -> Int -> Maybe Int
rangeMap rm n =
  if ss <= n && n < ss + rangeLen rm
    then Just (destStart rm + n - ss)
    else Nothing
  where
    ss = srcStart rm

data Mapping = Mapping
  { dest :: String,
    source :: String,
    ranges :: [RangeMapping]
  }
  deriving (Eq, Show)

mappingMap :: Int -> Mapping -> Int 
mappingMap n m = head (mapMaybe (`rangeMap` n) (ranges m) ++ [n])

type Problem = ([Int], [Mapping])

parse :: String -> Problem
parse s =
  (seeds, mappings)
  where
    (seedLine : theRest) = splitOn "\n\n" s
    seeds = map read . words . drop 6 $ seedLine
    mappings = map parseMapping theRest

-- | Parses one mapping
-- Example input:
-- seed-to-soil map:
-- 50 98 2
-- 52 50 48
parseMapping :: String -> Mapping
parseMapping s =
  Mapping {source = src, dest = dst, ranges = ranges}
  where
    (firstLine : restLines) = lines s
    [src, dst] = splitOn "-to-" . dropEnd 5 $ firstLine
    ranges = map parseRangeMapping restLines 
    parseRangeMapping s = 
      RangeMapping { destStart = read a, srcStart = read b, rangeLen = read c }
      where 
        [a, b, c] = words s

part1 :: Problem -> Int
part1 problem = minimum . map (mapToLocation problem "seed") . fst $ problem

mapToLocation :: Problem -> String -> Int -> Int 
mapToLocation _ "location" n = n 
mapToLocation problem from n =
  mapToLocation problem (dest m) (mappingMap n m)
  where 
    m = fromJust $ find ((== from) . source) (snd problem)

part2 :: Problem -> Int
part2 = length
