{-# LANGUAGE TupleSections #-}

module Main where

import Advent (runTestAndInput)
import Data.List.Extra (notNull)

main :: IO ()
main = runTestAndInput parse part1 part2

type Problem = [Int]

parse :: String -> [Int]
parse = map read . lines

part1 :: Problem -> Int
part1 =
  fst . head . filter (not . uncurry hasPairThatSumsTo) . drop 5 . makeSequence

part2 :: Problem -> Int
part2 = length

-- | Makes a sequence of (current, ringBuffer)
makeSequence :: [a] -> [(a, [a])]
makeSequence xs = zip xs (scanl (flip rbInsert) [] xs)

-- | Inserts an item in a ring buffer
rbInsert :: a -> [a] -> [a]
rbInsert x rb = x : take 4 rb

-- | Is there a pair that sums to the given number?
hasPairThatSumsTo :: Int -> [Int] -> Bool
hasPairThatSumsTo n = notNull . filter ((== n) . uncurry (+)) . allPairs

-- | Returns all of the pairs of two different items in a list, in the same order as they appear in the list.
allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs [_] = []
allPairs (x : xs) = map (x,) xs ++ allPairs xs
