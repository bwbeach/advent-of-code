module Main where

import Advent (run)
import Data.Char (isDigit)
import Data.List (group)
import Debug.Trace

main :: IO ()
main = run parse part1 part2

type Problem = String

parse :: String -> Problem
parse = filter isDigit

part1 :: Problem -> Int
part1 = length . (!! 40) . iterate seeAndSay

part2 :: Problem -> Int
part2 = length

-- | The part 1 action
--
-- >>> seeAndSay "1211"
-- "111221"
--
-- >>> seeAndSay "111221"
-- "312211"
seeAndSay :: String -> String
seeAndSay =
  concatMap doOne . group
  where
    doOne g = show (length g) ++ [head g]
