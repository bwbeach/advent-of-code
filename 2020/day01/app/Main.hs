module Main where

import Advent (message)
import Data.Bits (Bits (xor))
import GHC.IO.Exception (IOException (ioe_filename))

main :: IO ()
main = do
  runInput "test.txt"
  runInput "input.txt"

runInput :: String -> IO ()
runInput fileName = do
  putStrLn fileName
  text <- readFile fileName
  let input = parse text
  print . solve 2 $ input
  print . solve 3 $ input

parse :: String -> [Int]
parse = map read . lines

-- | Solves 2020 Day 01
--
-- The strategy is to generate all ways to choose n numbers from the
-- list, then filter down to the one set that adds up to 2020.
solve :: Int -> [Int] -> Int
solve n =
  product . only . filter sumIs2020 . choose n
  where
    sumIs2020 = (== 2020) . sum

-- | Returns combinations of n items from the list
--
-- Preserves the order of the items in the list, and does not
-- return duplicates that are the same items in a different
-- order.
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x : xs) = [x : ys | ys <- choose (n - 1) xs] ++ choose n xs

-- | Returns the contents if a singleton list.
--
-- It's an error if the length of the list is not 1.
only :: (Show a) => [a] -> a
only [a] = a
only as = error ("expected exactly one: " ++ show as)
