module Main where

import Advent (runTestAndInput)
import Data.List.Split

main :: IO ()
main = runTestAndInput parse part1 part2

type Color = String

type Rule = (Color, [(Int, Color)])

type Problem = [Rule]

parse :: String -> Problem
parse =
  map parseLine . endBy "\n"

-- | Parse one input line
-- Examples:
--    dotted black bags contain no other bags.
--    vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
parseLine :: String -> Rule
parseLine line =
  (bagColor left, parseRhs right)
  where
    parseRhs "no other bags." = []
    parseRhs s = map parseContent . splitOn "," $ s

    parseContent s =
      (read count, bagColor . unwords $ rest)
      where
        (count : rest) = words s

    [left, right] = splitOn " contain " line

    -- "bright red bags" -> "bright red"
    bagColor = unwords . take 2 . words

part1 :: Problem -> Problem
part1 = id

part2 :: Problem -> Int
part2 = length
