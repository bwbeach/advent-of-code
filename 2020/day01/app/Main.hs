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
  print . part1 $ input
  print . part2 $ input

parse :: String -> [Int]
parse = map read . lines

part1 :: [Int] -> Int
part1 = solve 2

part2 :: [Int] -> Int
part2 = solve 3

solve :: Int -> [Int] -> Int
solve n xs = theOne [product ys | ys <- choose n xs, sum ys == 2020]

choose :: Int -> [Int] -> [[Int]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x : xs) = [x : ys | ys <- choose (n - 1) xs] ++ choose n xs

theOne :: (Show a) => [a] -> a
theOne [a] = a
theOne as = error ("expected exactly one: " ++ show as)
