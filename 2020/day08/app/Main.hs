module Main where

import Advent (runTestAndInput)
import Data.List.Split (endBy)
import qualified Data.Map.Strict as M

main :: IO ()
main = runTestAndInput parse part1 part2

data Instruction
  = Acc Int
  | Jmp Int
  | Nop
  deriving (Show)

-- | Map from address to instruction at that address.
-- The first instruction is at address 1.
type Problem = M.Map Int Instruction

parse :: String -> Problem
parse =
  M.fromList . zip [1 ..] . map parseLine . lines
  where
    parseLine = parseWords . words

    parseWords ["acc", n] = Acc (parseNum n)
    parseWords ["jmp", n] = Jmp (parseNum n)
    parseWords ["nop", _] = Nop

    parseNum ('+' : n) = read n
    parseNum n = read n

part1 :: Problem -> Int
part1 = length

part2 :: Problem -> Int
part2 = length
