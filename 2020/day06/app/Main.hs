module Main where

import Advent (run)
import Data.List.Split (endBy, splitOn)
import qualified Data.Set as S

main :: IO ()
main = run parse part1 part2

type Problem = [[String]]

parse :: String -> Problem
parse = map (endBy "\n") . splitOn "\n\n"

part1 :: Problem -> Int
part1 = sum . map (S.size . S.fromList . concat)

part2 :: Problem -> Int
part2 = sum . map (S.size . foldr1 S.intersection . map S.fromList)
