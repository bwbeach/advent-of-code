module Main where

import Advent (run)
import Data.Tuple (swap)
import Topograph (pairs)

main :: IO ()
main = run parse part1 part2

type Problem = [[Int]]

parse :: String -> Problem
parse = map parseLine . lines
  where
    parseLine = map read . words

part1 :: Problem -> Int
part1 = sum . map extrapolateOne

extrapolateOne :: [Int] -> Int
extrapolateOne ns =
  if all (== 0) ns
    then 0
    else last ns + extrapolateOne (differences ns)

differences :: [Int] -> [Int]
differences = map (uncurry (-) . swap) . pairs

part2 :: Problem -> Int
part2 = sum . map extrapolateBack

extrapolateBack :: [Int] -> Int
extrapolateBack ns =
  if all (== 0) ns
    then 0
    else head ns - extrapolateBack (differences ns)
