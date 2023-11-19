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

data Ship = Ship {loc :: Point, dir :: Point} deriving (Show)

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

step :: Ship -> Instruction -> Ship
step ship inst =
  case inst of
    R n -> ship {dir = right (dir ship) n}
    F n -> ship {loc = loc ship + fmap (* n) (dir ship)}
    Move d -> ship {loc = loc ship + d}
  where
    right x 0 = x
    right (V2 x y) n = right (V2 y (-x)) (n - 90)

part1 :: Problem -> Int
part1 =
  manhattan . loc . foldl step start
  where
    start = Ship {loc = V2 0 0, dir = V2 1 0}
    manhattan (V2 x y) = abs x + abs y

part2 :: Problem -> Int
part2 = length
