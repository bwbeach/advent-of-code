module Main where

import Advent (run)
import Data.List (isInfixOf)

main :: IO ()
main = run parse part1 part2

type Problem = [String]

parse :: String -> Problem
parse = lines

part1 :: Problem -> Int
part1 = length . filter isNice

part2 :: Problem -> Int
part2 = length . filter isNice2

-- | Is the given string nice?
--
-- >>> isNice "ugknbfddgicrmopn"
-- True
--
-- >>> isNice "aaa"
-- True
--
-- >>> isNice "jchzalrnumimnmhp"
-- False
--
-- >>> isNice "haegwjzuvuyypxyu"
-- False
--
-- >>> isNice "dvszwmarrgswjxmb"
-- False
isNice :: String -> Bool
isNice s =
  3 <= vowelCount s && hasDouble s && not (containsAnyOf s ["ab", "cd", "pq", "xy"])
  where
    vowelCount = length . filter (`elem` "aeiou")

    hasDouble (a : b : cs) = a == b || hasDouble (b : cs)
    hasDouble _ = False

    containsAnyOf haystack = any (`isInfixOf` haystack)

-- | Is the string nice in part 2?
--
-- >>> isNice2 "qjhvhtzxzqqjkmpb"
-- True
--
-- >>> isNice2 "uurcxstgmygtbstg"
-- False
--
-- >>> isNice2 "ieodomkazucvgmuy"
-- False
isNice2 :: String -> Bool
isNice2 s =
  hasRepeatedPair s && hasPairWithSpace s
  where
    hasRepeatedPair (a : b : cs) = [a, b] `isInfixOf` cs || hasRepeatedPair (b : cs)
    hasRepeatedPair _ = False

    hasPairWithSpace (a : b : c : ds) = a == c || hasPairWithSpace (b : c : ds)
    hasPairWithSpace _ = False
