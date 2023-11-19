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
  = Mask String
  | Store Integer Integer
  deriving (Eq, Show)

type Problem = [Instruction]

parse :: String -> Problem
parse =
  map parseLine . lines

parseLine :: String -> Instruction
parseLine line =
  if left == "mask"
    then Mask right
    else Store addr value
  where
    [left, right] = splitOn " = " line
    addr = read . drop 4 . dropEnd 1 $ left
    value = read right

data Masker = Masker {orMask :: Integer, andMask :: Integer} deriving (Eq, Show)

masker :: String -> Masker
masker s =
  Masker
    { orMask = readBinary . replace 'X' '0' $ s,
      andMask = readBinary . replace 'X' '1' $ s
    }

applyMasker :: Masker -> Integer -> Integer
applyMasker m n = (n .&. andMask m) .|. orMask m

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
    (finalMap, _) = foldl' step1 (M.empty, masker "") problem

step1 :: (M.Map Integer Integer, Masker) -> Instruction -> (M.Map Integer Integer, Masker)
step1 (m, _) (Mask mask) = (m, masker mask)
step1 (m, masker) (Store a v) = (M.insert a (applyMasker masker v) m, masker)

part2 :: Problem -> Integer
part2 problem =
  sum . M.elems $ finalMap
  where
    (finalMap, _) = foldl' step2 (M.empty, []) problem

step2 :: (M.Map Integer Integer, [Masker]) -> Instruction -> (M.Map Integer Integer, [Masker])
step2 (m, _) (Mask mask) =
  (m, map masker (allMasks mask))
  where
    allMasks [] = [""]
    allMasks ('0' : ms) = map ('X' :) (allMasks ms)
    allMasks ('1' : ms) = map ('1' :) (allMasks ms)
    allMasks ('X' : ms) = map ('0' :) (allMasks ms) ++ map ('1' :) (allMasks ms)
step2 (m, maskers) (Store a v) =
  (foldl' doOne m maskers, maskers)
  where
    doOne m0 masker = M.insert (applyMasker masker a) v m0
