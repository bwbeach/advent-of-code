module Main where

import Advent (run)
import Data.List.Extra (dropEnd, find, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)

main :: IO ()
main = run parse part1 part2

type Range = (Int, Int)

rangeContains :: Range -> Int -> Bool
rangeContains (a, b) n = a <= n && n <= b

-- | Holds the mapping for a range of values.
-- Represented as a range and the offset to add to values in that range
type RangeMapping = (Range, Int)

-- | Use a range mapping to map one number, if it's in range.
--
-- >>> rangeMap ((14, 16), (-10)) 15
-- Just 5
--
-- >>> rangeMap ((14, 16), (-10)) 17
-- Nothing
rangeMap :: RangeMapping -> Int -> Maybe Int
rangeMap (r, d) n =
  if rangeContains r n
    then Just (n + d)
    else Nothing

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
  Mapping {source = src, dest = dst, ranges = sort ranges}
  where
    (firstLine : restLines) = lines s
    [src, dst] = splitOn "-to-" . dropEnd 5 $ firstLine
    ranges = map parseRangeMapping restLines
    parseRangeMapping s =
      ((srcStart, srcStart + rangeLen - 1), destStart - srcStart)
      where
        [a, b, c] = words s
        destStart = read a 
        srcStart = read b 
        rangeLen = read c

part1 :: Problem -> Int
part1 problem = minimum . map (mapToLocation problem "seed") . fst $ problem

mapToLocation :: Problem -> String -> Int -> Int
mapToLocation _ "location" n = n
mapToLocation problem from n =
  mapToLocation problem (dest m) (mappingMap n m)
  where
    m = fromJust $ find ((== from) . source) (snd problem)

part2 :: Problem -> Int
part2 problem = 0
  -- minimum . map (mapToLocation problem "seed") . concatMap convertRange . pairs . fst $ problem
  -- where
  --   startingRanges = pairs . fst $ problem
  --   convertRange (a, b) = [a .. (a + b - 1)]
  --   pairs [] = []
  --   pairs (a : b : cs) = (a, b) : pairs cs

-- | Given a list of ranges and a Mapping, return the result of mapping all the numbers in the ranges as a list of ranges. 
-- mapRanges ::  Mapping -> [(Int, Int)] -> [(Int, Int)]
-- mapRanges mapping = normalizeRanges . concatMap (mapRange mapping)

-- mapRange :: Mapping -> (Int, Int) -> [(Int, Int)]
-- mapRange mapping r =
--   go r (ranges mapping) 
--   where 
--     go r [] = [r] 
--     go (a, b) {}

-- | Given a list of ranges (represented as pairs), resolves overlaps
--
-- >>> normalizeRanges [(1, 4), (3, 9)]
-- [(1,9)]
--
-- >>> normalizeRanges [(5, 8), (1, 4)]
-- [(1,4),(5,8)]
normalizeRanges :: (Ord a) => [(a, a)] -> [(a, a)]
normalizeRanges =
  go . sort
  where
    go [] = []
    go [r] = [r]
    go ((a, b) : (c, d) : rs)
      | b < c = (a, b) : go ((c, d) : rs)
      | otherwise = go ((a, max b d) : rs)
