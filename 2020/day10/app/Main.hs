module Main where

import Advent (runTestAndInput)
import Data.List (group, sort)
import qualified Data.Map.Strict as M
import Debug.Trace
import Topograph (pairs)

main :: IO ()
main = runTestAndInput parse part1 part2

type Problem = [Int]

parse :: String -> Problem
parse = map read . lines

part1 :: Problem -> Int
part1 problem =
  m M.! 1 * m M.! 3
  where
    m = M.fromList . map valueAndCount . group . sort . map delta . pairs . sort . addBounds $ problem
    addBounds problem = problem ++ [0, 3 + maximum problem]
    valueAndCount as = (head as, length as)
    delta (a, b) = b - a

part2 :: Problem -> Int
part2 = length
