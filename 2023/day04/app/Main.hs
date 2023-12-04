module Main where

import Advent (run)
import qualified Data.Set as S
import Data.List.Split (splitOn)

main :: IO ()
main = run parse part1 part2

type Card = (Int, S.Set Int, S.Set Int)

type Problem = [Card]

-- 
parse :: String -> Problem
parse = map parseLine . lines

-- | Parses one line that looks like this:
-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
parseLine :: String -> Card 
parseLine s =
    (read . drop 5 $ a, winners, have)
    where 
        [a, b] = splitOn ": " s
        [c, d] = splitOn " | " b 
        winners = S.fromList . map read . words $ c 
        have = S.fromList . map read . words $ d

part1 :: Problem -> Int
part1 = 
    sum . map cardScore 
    where 
         cardScore (_, w, h) = exp2 . length . filter (`S.member` w) . S.toList $ h 

         exp2 0 = 0
         exp2 1 = 1
         exp2 n = 2 * exp2 (n - 1)

part2 :: Problem -> Int
part2 = length
