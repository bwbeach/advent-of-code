module Main where

import Advent (runTestAndInput)
import Data.List.Split (splitOn)

main :: IO ()
main = runTestAndInput parse id id

-- | Parses puzzle input line by line.
parse :: String -> [(Int, Int, Char, String)]
parse = map parseLine . lines

-- | Parses one line of input.
--
-- Example input lines:
--   15-16 g: gggggggggggggggv
--   6-9 t: ftjztttfnztv
parseLine :: String -> (Int, Int, Char, String)
parseLine s =
  (a, b, c, p)
  where
    [nums, middle, p] = splitOn " " s
    c = head middle
    [a, b] = map read . splitOn "-" $ nums
