module Main where

import Advent (runTestAndInput)
import Data.List.Split

main :: IO ()
main = runTestAndInput parse part1 part2

parse :: String -> [String]
parse = endBy "\n"

part1 :: [String] -> Int
part1 =
  maximum . map seatId
  where
    seatId = foldl go 0
    go n c = n * 2 + charVal c
    charVal c = if c `elem` "RB" then 1 else 0

part2 _ = 5
