module Main where

import Advent (run)
import Data.Char (ord)
import Data.List (foldl')
import Data.List.Split (splitOn)

main :: IO ()
main = run parse part1 part2

type Problem = [String]

parse :: String -> Problem
parse = splitOn "," . filter (/= '\n')

part1 :: Problem -> Int
part1 = sum . map hash

part2 :: Problem -> Int
part2 = length

hash :: String -> Int
hash =
  foldl' hashChar 0
  where
    hashChar h c = ((h + ord c) * 17) `mod` 256
