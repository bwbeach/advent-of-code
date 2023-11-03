{-# LANGUAGE TupleSections #-}

module Main where

import Advent (runTestAndInput)
import Data.List.Extra (notNull)

main :: IO ()
main = runTestAndInput parse part1 part2

type Problem = (Int, [Int])

parse :: String -> Problem
parse s =
  (head ns, tail ns)
  where
    ns = map read . lines $ s

part1 :: Problem -> Int
part1 (bufferSize, ns) =
  fst . head . filter (not . uncurry hasPairThatSumsTo) . drop bufferSize . makeSequence bufferSize $ ns

part2 :: Problem -> Int
part2 = length

-- | Makes a sequence of (current, ringBuffer)
makeSequence :: Int -> [a] -> [(a, [a])]
makeSequence bufferSize xs = zip xs (scanl (flip (rbInsert bufferSize)) [] xs)

-- | Inserts an item in a ring buffer
rbInsert :: Int -> a -> [a] -> [a]
rbInsert bufferSize x rb = x : take (bufferSize - 1) rb

-- | Is there a pair that sums to the given number?
hasPairThatSumsTo :: Int -> [Int] -> Bool
hasPairThatSumsTo n = notNull . filter ((== n) . uncurry (+)) . allPairs

-- | Returns all of the pairs of two different items in a list, in the same order as they appear in the list.
allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs [_] = []
allPairs (x : xs) = map (x,) xs ++ allPairs xs
