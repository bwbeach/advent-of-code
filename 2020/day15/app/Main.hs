module Main where

import Advent (run)
import Data.List (find)
import Data.List.Extra (dropEnd)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Debug.Trace

main :: IO ()
main = run parse part1 part2

type Problem = [Int]

parse :: String -> Problem
parse = map read . splitOn ","

part1 :: Problem -> Int
part1 problem =
  middle . fromJust . find ((== 2020) . left) . iterate step1 $ initialState
  where
    initialState = (length problem, last problem, M.fromList $ zip (dropEnd 1 problem) [1 ..])
    left (x, _, _) = x
    middle (_, x, _) = x

step1 :: (Int, Int, M.Map Int Int) -> (Int, Int, M.Map Int Int)
step1 (i, n, m) =
  (i + 1, n', M.insert n i m)
  where
    n' = case M.lookup n m of
      Nothing -> 0
      Just j -> i - j

part2 :: Problem -> Int
part2 = length
