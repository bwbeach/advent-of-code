{-# LANGUAGE TupleSections #-}

module Main where

import Advent (run)
import Data.List.Extra (notNull, tails)
import Data.Maybe (mapMaybe)

main :: IO ()
main = run parse part1 part2

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
part2 problem =
  minimum contiguous + maximum contiguous
  where
    p1 = part1 problem
    contiguous = only . filter ((2 <=) . length) . mapMaybe (headThatAddsTo p1) . tails . snd $ problem

    headThatAddsTo :: Int -> [Int] -> Maybe [Int]
    headThatAddsTo 0 _ = Just []
    headThatAddsTo _ [] = Nothing
    headThatAddsTo n (x : xs) =
      if x <= n
        then (x :) <$> headThatAddsTo (n - x) xs
        else Nothing

    only [x] = x
    only xs = error ("expected exactly one item in list: " ++ show xs)

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
