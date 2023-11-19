module Main where

import Advent (run)
import Data.Bits ((.&.), (.|.))
import Data.List.Extra (dropEnd, foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main = run parse part1 part2

data Instruction
  = Mask Integer Integer
  | Store Integer Integer
  deriving (Eq, Show)

type Problem = [Instruction]

parse :: String -> Problem
parse =
  map parseLine . lines

parseLine :: String -> Instruction
parseLine line =
  if left == "mask"
    then Mask andMask orMask
    else Store addr value
  where
    [left, right] = splitOn " = " line
    addr = read . drop 4 . dropEnd 1 $ left
    value = read right
    andMask = readBinary . replace 'X' '1' $ right
    orMask = readBinary . replace 'X' '0' $ right

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)

readBinary :: String -> Integer
readBinary =
  go 0
  where
    go n [] = n
    go n (d : xs) = go (n * 2 + digit d) xs

    digit '0' = 0
    digit '1' = 1

part1 :: Problem -> Integer
part1 problem =
  sum . M.elems $ finalMap
  where
    (finalMap, _, _) = foldl' step1 (M.empty, 0, 0) problem

step1 :: (M.Map Integer Integer, Integer, Integer) -> Instruction -> (M.Map Integer Integer, Integer, Integer)
step1 (m, _, _) (Mask am om) = (m, am, om)
step1 (m, am, om) (Store a v) = (M.insert a ((v .&. am) .|. om) m, am, om)

part2 :: Problem -> Int
part2 = length
