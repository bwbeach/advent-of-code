module Main where

import Advent (runTestAndInput)
import Data.List.Split (splitOn)

main :: IO ()
main = runTestAndInput parse (solve isValid1) (solve isValid2)

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

-- | Runs day02 on one input
solve :: ((Int, Int, Char, String) -> Bool) -> [(Int, Int, Char, String)] -> Int
solve isValid = length . filter isValid

-- | Is one password valid in part 1?
isValid1 :: (Int, Int, Char, String) -> Bool
isValid1 (a, b, c, p) =
  a <= n && n <= b
  where
    n = length . filter (== c) $ p

-- | Is one password valid in part 2?
isValid2 :: (Int, Int, Char, String) -> Bool
isValid2 (a, b, c, p) =
  xor ((p !! (a - 1)) == c) ((p !! (b - 1)) == c)
  where
    xor a b = (a && not b) || (not a && b)
