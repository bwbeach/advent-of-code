module Main where

import Advent (runTestAndInput)
import Data.List.Split (endBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace

main :: IO ()
main = runTestAndInput parse part1 part2

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
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
    parseWords ["nop", n] = Nop (parseNum n)

    parseNum ('+' : n) = read n
    parseNum n = read n

-- | The state of the computer is (addr, acc)
-- The addr is the next instruction to run, and the acc is the accumulator
-- just before running that instruction.
type State = (Int, Int)

-- | Runs the next instruction, producing the new state.
step :: Problem -> State -> State
step problem (addr, acc) =
  runInstruction . fromJust $ M.lookup addr problem
  where
    runInstruction (Acc n) = (addr + 1, acc + n)
    runInstruction (Jmp n) = (addr + n, acc)
    runInstruction (Nop _) = (addr + 1, acc)

part1 :: Problem -> Int
part1 problem =
  go S.empty (1, 0)
  where
    go visited (addr, acc) =
      if S.member addr visited
        then acc
        else go (S.insert addr visited) (addr', acc')
      where
        (addr', acc') = step problem (addr, acc)

part2 :: Problem -> Int
part2 = length
