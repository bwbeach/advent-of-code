module Main where

import Advent (run)
import Data.List.Split (splitOn)

main :: IO ()
main = run parse part1 part2

type Problem = [(Int, Int, Int)]

-- | Parse the input file
--
-- >>> parse "2x3x4\n5x6x7\n"
-- [(2,3,4),(5,6,7)]
parse :: String -> Problem
parse =
  map parseLine . lines
  where
    parseLine = listToTriple . map read . splitOn "x"
    listToTriple [a, b, c] = (a, b, c)
    listToTriple x = error ("expected list of three: " ++ show x)

part1 :: Problem -> Int
part1 =
  sum . map paperArea
  where
    paperArea (l, w, h) = 2 * l * w + 2 * l * h + 2 * w * h + minimum [l * w, l * h, w * h]

part2 :: Problem -> Int
part2 =
  sum . map ribbonLength
  where
    ribbonLength (l, w, h) = 2 * minimum [l + w, l + h, w + h] + l * w * h
