module Main where

import Advent (run)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Debug.Trace
import GHC.Event.TimeOut (TimeoutKey (TK))

main :: IO ()
main = run parse part1 part2

-- | A list of the rows of text in one grid of ash ('.') and rocks ('#')
type Pattern = [String]

type Problem = [Pattern]

parse :: String -> Problem
parse = splitOn [""] . lines

part1 :: Problem -> Int
part1 = goPart 0

part2 :: Problem -> Int
part2 = goPart 1

goPart :: Int -> Problem -> Int
goPart expectedSmudges =
  sum . map summary
  where
    summary p = 100 * sumOne p + sumOne (transpose p)
    sumOne = reflectionSum expectedSmudges

-- | The sum of reflection scores
reflectionSum :: Int -> [String] -> Int
reflectionSum expectedSmudges =
  sum . map (length . fst) . filter ((== expectedSmudges) . smudgeCount) . filter bothNonEmpty . candidates
  where
    bothNonEmpty (_ : _, _ : _) = True
    bothNonEmpty (_, _) = False

    isReflection as@(_ : _) bs@(_ : _) = and $ zipWith (==) as bs
    isReflection _ _ = False

-- | Count the smudges for a reflection candidate
--
-- >>> smudgeCount (["#.#", "..#"], ["#..", ".##"])
-- 2
smudgeCount :: ([String], [String]) -> Int
smudgeCount (as, bs) =
  sum $ zipWith rowCount as bs
  where
    rowCount a b = length . filter id $ zipWith (/=) a b

-- | All of the candidate pairs that might be reflections.
candidates :: [String] -> [([String], [String])]
candidates as =
  go [] as
  where
    go revBefore after = (revBefore, after) : go' revBefore after

    go' _ [] = []
    go' revBefore (a : as) = go (a : revBefore) as
