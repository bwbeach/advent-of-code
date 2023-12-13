module Main where

import Advent (run)
import Data.List (transpose)
import Data.List.Split (splitOn)

main :: IO ()
main = run parse part1 part2

-- | A list of the rows of text in one grid of ash ('.') and rocks ('#')
type Pattern = [String]

type Problem = [Pattern]

parse :: String -> Problem
parse = splitOn [""] . lines

part1 :: Problem -> Int
part1 =
  sum . map summary
  where
    summary p = 100 * reflectionSum p + reflectionSum (transpose p)

-- | The sum of reflection scores
reflectionSum :: (Eq a) => [a] -> Int
reflectionSum =
  sum . map (length . fst) . filter (uncurry isReflection) . candidates
  where
    isReflection as@(_ : _) bs@(_ : _) = all (uncurry (==)) $ zip as bs
    isReflection _ _ = False

-- | All of the candidate pairs that might be reflections.
candidates :: (Eq a) => [a] -> [([a], [a])]
candidates as =
  go [] as
  where
    go revBefore after = (revBefore, after) : go' revBefore after

    go' _ [] = []
    go' revBefore (a : as) = go (a : revBefore) as

part2 :: Problem -> Int
part2 = length
