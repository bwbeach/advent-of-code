module Main where

import Advent
  ( Point,
    run,
  )
import Data.Bits (Bits (xor))
import Debug.Trace
import Linear (V2 (..))

main :: IO ()
main = run parse part1 part2

data Instruction
  = Move Point
  | R Int
  | L Int
  | F Int
  deriving (Show)

type Problem = [Instruction]

parse :: String -> Problem
parse =
  map parseInstruction . lines
  where
    parseInstruction (c : ns) = parseHelper c (read ns)

    parseHelper 'R' n = R n
    parseHelper 'L' n = R (360 - n)
    parseHelper 'F' n = F n
    parseHelper 'N' n = Move $ fmap (* n) (V2 0 1)
    parseHelper 'S' n = Move $ fmap (* n) (V2 0 (-1))
    parseHelper 'W' n = Move $ fmap (* n) (V2 (-1) 0)
    parseHelper 'E' n = Move $ fmap (* n) (V2 1 0)
    parseHelper c n = error ("bad input: " ++ [c] ++ show n)

data Ship1 = Ship1 {loc :: Point, dir :: Point} deriving (Show)

step :: Ship1 -> Instruction -> Ship1
step ship inst =
  case inst of
    R n -> ship {dir = right (dir ship) n}
    F n -> ship {loc = loc ship + fmap (* n) (dir ship)}
    Move d -> ship {loc = loc ship + d}

right :: Point -> Int -> Point
right x 0 = x
right (V2 x y) n = right (V2 y (-x)) (n - 90)

manhattan :: Point -> Int
manhattan (V2 x y) = abs x + abs y

part1 :: Problem -> Int
part1 =
  manhattan . loc . foldl step start
  where
    start = Ship1 {loc = V2 0 0, dir = V2 1 0}

data Ship2 = Ship2 {pos :: Point, wpt :: Point} deriving (Show)

step2 :: Ship2 -> Instruction -> Ship2
step2 ship inst =
  case inst of
    R n -> ship {wpt = right (wpt ship) n}
    F n -> ship {pos = pos ship + fmap (* n) (wpt ship)}
    Move d -> ship {wpt = wpt ship + d}

part2 :: Problem -> Int
part2 =
  manhattan . pos . foldl step2 start
  where
    start = Ship2 {pos = V2 0 0, wpt = V2 10 1}
    tstep2 a b = traceShowId (step2 a b)
