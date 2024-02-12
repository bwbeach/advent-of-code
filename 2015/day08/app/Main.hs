module Main where

import Advent (run)
import Data.List.Extra (dropEnd)

main :: IO ()
main = run parse part1 part2

type Problem = [String]

parse :: String -> Problem
parse = lines

part1 :: Problem -> Int
part1 =
  sum . map lineDiff
  where
    lineDiff s = length s - decodedLength s

-- | How long is a string once it's decoded?
-- >>> decodedLength "\"A\xd6Z\""
-- 3
decodedLength :: String -> Int
decodedLength =
  go . drop 1 . dropEnd 1
  where
    go [] = 0
    go ('\\' : 'x' : _ : _ : cs) = 1 + go cs
    go ('\\' : '"' : cs) = 1 + go cs
    go ('\\' : '\\' : cs) = 1 + go cs
    go (_ : cs) = 1 + go cs

part2 :: Problem -> Int
part2 = length
