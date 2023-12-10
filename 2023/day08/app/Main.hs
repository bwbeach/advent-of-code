module Main where

import Advent (only, run)
import Data.Function ((&))
import Data.List.Extra (dropEnd, foldl', scanl', takeEnd)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tuple (swap)

main :: IO ()
main = run parse part1 part2

type Problem = (String, M.Map String (String, String))

parse :: String -> Problem
parse s =
  (turns, mapping)
  where
    [turns, mapText] = splitOn "\n\n" s
    mapping = M.fromList . map parseOneMap . lines $ mapText

parseOneMap :: String -> (String, (String, String))
parseOneMap s =
  (from, (left, right))
  where
    [from, choices] = splitOn " = " s
    [left, right] = splitOn ", " . drop 1 . dropEnd 1 $ choices

part1 :: Problem -> Int
part1 problem@(turns, mapping) =
  if not (M.member "AAA" mapping)
    then 0 -- hack for second test case, which doesn't work for part 1
    else length . takeWhile (/= "ZZZ") . scanl' (oneMove problem) "AAA" . cycle $ turns

oneMove :: Problem -> String -> Char -> String
oneMove (_, mapping) from turn = (if turn == 'L' then fst else snd) (mapping M.! from)

isA :: String -> Bool
isA = (== "A") . takeEnd 1

isZ :: String -> Bool
isZ = (== "Z") . takeEnd 1

-- | Given the name of a node, finds the first "Z" it gets to, and the length of the repeat.
--
-- The result is (stepsToFirstZ, stepsToNextZ)
--
-- It's not clear to me that every input would have the property that each starting point
-- results in a "Z" that repeats every N steps, but that seems to be the case with the
-- input for this problem.  As a double-check, this finds the first 5 repeats, and checks
-- that the interval is the same for each one.
repeatInterval :: Problem -> String -> (Integer, Integer)
repeatInterval problem@(turns, mapping) start =
  -- start with the sequence of left and right turns
  turns
    -- make it an infinitely repeating sequence of turns
    & cycle
    -- starting with `start`, convert the turns into a list of nodes visited: [nodeName]
    & scanl' (oneMove problem) start
    -- add a step number to each one: [(stepIndex, nodeName)]
    & zip [0 ..]
    -- keep only the nodes whose names end in Z: [(stepIndex, nodeName)]
    & filter (isZ . snd)
    -- we only need the step indexes [stepIndex]
    & map fst
    -- convert to the number of steps between
    & extractRepeatInfo

-- | Given a list of times that something happens, return the (firstTime, repeatInterval)
--
-- Double checks that the first five repeats do, in fact, happen at regular intervals
-- by getting unique values and checking that there is `only` one.
--
-- >>> extractRepeatInfo (iterate (+ 5) 2)
-- (2,5)
extractRepeatInfo :: [Int] -> (Integer, Integer)
extractRepeatInfo ts =
  (toInteger $ head ts, toInteger interval)
  where
    interval = only . uniqueValues . take 5 . map (uncurry (-) . swap) . pairs $ ts
    uniqueValues = S.toList . S.fromList

part2 :: Problem -> Integer
part2 problem@(turns, mapping) =
  -- start with all of the nodes whose names end in A
  allA
    -- get the time to first Z and cycle time
    & map (repeatInterval problem)
    -- merge the intervals
    & foldl' mergeIntervals (1, 1)
    -- grab the repeat cycle
    & fst
  where
    allA = filter isA . M.keys $ mapping

-- | Given two instances of (first, repeatTime), what's the intersection?
--
-- >>> mergeIntervals (2, 7) (10, 3)
-- (16,21)
--
-- >>> mergeIntervals (1, 1) (6, 1)
-- (6,1)
mergeIntervals :: (Integral b) => (b, b) -> (b, b) -> (b, b)
mergeIntervals (t0, p0) (t1, p1) =
  (nextMatch, lcm p0 p1)
  where
    nextMatch = head . filter isMatch . iterate (+ p0) $ t0
    isMatch t = (t - t1) `mod` p1 == 0 && t1 <= t

-- | All adjacent pairs in a list of things
--
-- >>> pairs [1, 2, 3]
-- [(1,2),(2,3)]
pairs [] = []
pairs [_] = []
pairs (a : b : cs) = (a, b) : pairs (b : cs)
