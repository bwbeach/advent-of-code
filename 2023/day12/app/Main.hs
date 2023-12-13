module Main where

import Advent (memoize, run, runMemoize)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Tuple.Extra (second)

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
  sum . map runOne
  where
    runOne (s, ns) = matchCount s (compilePattern ns)

part2 :: Problem -> Int
part2 =
  part1 . map unfold
  where
    unfold (a, b) = (intercalate "?" . replicate 5 $ a, concat . replicate 5 $ b)

data PatternElem
  = Dot -- exactly one '.'
  | Dots -- zero or more '.'
  | Hash -- exactly one '#'
  deriving (Eq, Ord, Show)

-- | Translate the list of numbers from the problem into a pattern
--
-- >>> compilePattern [2, 1]
-- [Dots,Hash,Hash,Dot,Dots,Hash,Dots]
compilePattern :: [Int] -> [PatternElem]
compilePattern ns = [Dots] ++ intercalate [Dot, Dots] (map (`replicate` Hash) ns) ++ [Dots]

-- | Return all of the strings matching the input string and the pattern
--
-- >>> matchCount "" []
-- 1
--
-- >>> matchCount "?" [Hash]
-- 1
--
-- >>> matchCount "???.###" [Dots, Hash, Dot, Dots, Hash, Dot, Dots, Hash, Hash, Hash, Dots]
-- 1
--
--
-- >>> matchCount ".??..??...?##." [Dots, Hash, Dot, Dots, Hash, Dot, Dots, Hash, Hash, Hash, Dots]
-- 4
matchCount :: String -> [PatternElem] -> Int
matchCount c p =
  runMemoize go (c, p)
  where
    go = memoize go'

    go' ("", []) = return 1
    go' ('.' : cs, Dot : ps) = go (cs, ps)
    go' ('?' : cs, Dot : ps) = go (cs, ps)
    go' ('#' : cs, Hash : ps) = go (cs, ps)
    go' ('?' : cs, Hash : ps) = go (cs, ps)
    go' (cs, Dots : ps) = do
      a <- go (cs, ps)
      b <- go (cs, Dot : Dots : ps)
      return (a + b)
    go' _ = return 0
