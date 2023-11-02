module Main where

import Advent (runTestAndInput)
import Data.List.Split (endBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)
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

-- | The state of the computer, including addresses of all instructions that have executed.
data State = State
  { pc :: Int,
    acc :: Int,
    history :: S.Set Int
  }
  deriving (Show)

-- | Runs the next instruction, producing the new state.
step :: Problem -> State -> State
step problem s =
  runInstruction $ M.lookup (pc s) problem
  where
    runInstruction Nothing = s
    runInstruction (Just (Acc n)) = s {pc = pc s + 1, acc = acc s + n, history = newHistory}
    runInstruction (Just (Jmp n)) = s {pc = pc s + n, history = newHistory}
    runInstruction (Just (Nop _)) = s {pc = pc s + 1, history = newHistory}

    newHistory = S.insert (pc s) (history s)

-- | The start state
initialState :: State
initialState = State {pc = 1, acc = 0, history = S.empty}

-- | The sequence of all states the program goes through
allStates :: Problem -> [State]
allStates problem = iterate (step problem) initialState

-- | Has a state looped?
hasLooped :: State -> Bool
hasLooped s = S.member (pc s) (history s)

-- | Has a state halted?
hasHalted :: Problem -> State -> Bool
hasHalted problem s = isNothing $ M.lookup (pc s) problem

part1 :: Problem -> Int
part1 = acc . head . dropWhile (not . hasLooped) . allStates

part2 :: Problem -> Int
part2 = length
