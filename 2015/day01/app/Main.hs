module Main where

import Advent (run)
import Data.List (findIndex)
import Data.Maybe (fromJust, mapMaybe)

main :: IO ()
main = run parse part1 part2

type Problem = [Int]

parse :: String -> Problem
parse = mapMaybe toDelta

-- | Convert one input character to a number of floors up (positive) or down (negative)
toDelta :: Char -> Maybe Int
toDelta '(' = Just 1
toDelta ')' = Just (-1)
toDelta c = error ("unknown input char: " ++ show c)

part1 :: Problem -> Int
part1 = sum

part2 :: Problem -> Int
part2 = fromJust . findIndex (< 0) . scanl (+) 0
