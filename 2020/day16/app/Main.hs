module Main where

import Advent (run)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main = run parse part1 part2

-- | A range of numbers, inclusive
data Range = Range Int Int deriving (Show)

-- | True iff the number is in the range.
inRange :: Int -> Range -> Bool
inRange n (Range a b) = a <= n && n <= b

-- | True if the number is in ANY of the ranges.
inRanges :: Int -> [Range] -> Bool
inRanges n = any (inRange n)

type Ticket = [Int]

data Problem = Problem
  { ranges :: M.Map String [Range],
    mine :: Ticket,
    nearby :: [Ticket]
  }
  deriving (Show)

parse :: String -> Problem
parse s =
  Problem {ranges = rs, mine = m, nearby = ns}
  where
    [a, b, c] = splitOn "\n\n" s
    rs = M.fromList . map parseRange . lines $ a
    m = parseTicket . last . lines $ b
    ns = map parseTicket . drop 1 . lines $ c

parseRange :: String -> (String, [Range])
parseRange s =
  (name, rs)
  where
    [name, rhs] = splitOn ": " s
    rs = map parseOneRange . splitOn "or" $ rhs

parseOneRange :: String -> Range
parseOneRange s =
  Range (read a) (read b)
  where
    [a, b] = splitOn "-" s

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

part1 :: Problem -> Int
part1 problem =
  sum . filter notInAnyRange . concat . nearby $ problem
  where
    notInAnyRange n = not . any (inRange n) $ allRanges
    allRanges = concat . ranges $ problem

part2 :: Problem -> Int
part2 = length . ranges
