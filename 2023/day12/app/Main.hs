module Main where

import Advent (run)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Tuple.Extra (second)
import Debug.Trace

main :: IO ()
main = run parse part1 part2

type Problem = [(String, [Int])]

parse :: String -> Problem
parse =
  map parseLine . lines
  where
    parseLine s =
      (a, ns)
      where
        [a, b] = splitOn " " s
        ns = map read . splitOn "," $ b

part1 :: Problem -> Int
part1 =
  sum . traceShowId . map runOne
  where
    runOne (s, ns) = length $ allMatches s (compilePattern ns)

part2 :: Problem -> Int
part2 =
  part1 . map unfold
  where
    unfold (a, b) = (intercalate "?" . replicate 5 $ a, concat . replicate 5 $ b)

data PatternElem
  = Dot -- exactly one '.'
  | Dots -- zero or more '.'
  | Hash -- exactly one '#'
  deriving (Eq, Show)

-- | Translate the list of numbers from the problem into a pattern
--
-- >>> compilePattern [2, 1]
-- [Dots,Hash,Hash,Dot,Dots,Hash,Dots]
compilePattern :: [Int] -> [PatternElem]
compilePattern ns = [Dots] ++ intercalate [Dot, Dots] (map (`replicate` Hash) ns) ++ [Dots]

-- | Return all of the strings matching the input string and the pattern
--
-- >>> allMatches "" []
-- [""]
--
-- >>> allMatches "?" [Hash]
-- ["#"]
--
-- >>> allMatches "???.###" [Dots, Hash, Dot, Dots, Hash, Dot, Dots, Hash, Hash, Hash, Dots]
-- ["#.#.###"]
--
--
-- >>> allMatches ".??..??...?##." [Dots, Hash, Dot, Dots, Hash, Dot, Dots, Hash, Hash, Hash, Dots]
-- WAS []
-- NOW [".#...#....###.",".#....#...###.","..#..#....###.","..#...#...###."]
allMatches :: String -> [PatternElem] -> [String]
allMatches "" [] = [""]
allMatches ('.' : cs) (Dot : ps) = map ('.' :) (allMatches cs ps)
allMatches ('?' : cs) (Dot : ps) = map ('.' :) (allMatches cs ps)
allMatches ('#' : cs) (Hash : ps) = map ('#' :) (allMatches cs ps)
allMatches ('?' : cs) (Hash : ps) = map ('#' :) (allMatches cs ps)
allMatches cs (Dots : ps) = allMatches cs ps ++ allMatches cs (Dot : Dots : ps)
allMatches _ _ = []
