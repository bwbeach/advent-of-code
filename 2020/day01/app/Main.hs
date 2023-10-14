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
part1 xs =
  a * b
  where
    possible = crossProduct xs xs
    theOne = filter sumsTo2020 possible
    sumsTo2020 (a, b) = a + b == 2020
    (a, b) = head theOne

part2 :: [Int] -> Int
part2 xs =
  a * b * c
  where
    possible = [(a, b, c) | a <- xs, b <- xs, c <- xs]
    theOne = filter sumsTo2020 possible
    sumsTo2020 (a, b, c) = a + b + c == 2020
    (a, b, c) = head theOne

crossProduct :: [a] -> [b] -> [(a, b)]
crossProduct as bs =
  [(a, b) | a <- as, b <- bs]
