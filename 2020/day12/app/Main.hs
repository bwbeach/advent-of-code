module Main where

import Advent (run)

main :: IO ()
main = run parse part1 part2

type Problem = String

parse :: String -> Problem
parse = id

part1 :: Problem -> Int
part1 = length

part2 :: Problem -> Int
part2 = length
