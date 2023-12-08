module Main where

import Advent (run)
import GHC.Float

main :: IO ()
main = run parse part1 part2

type Race = (Int, Int)  -- time and distance 

type Problem = [Race]

-- | parse one input file
parse :: String -> Problem
parse s = 
    zip times distances
    where
        [a, b] = lines s 
        times = map read . tail . words $ a 
        distances = map read . tail. words $ b

part1 :: Problem -> Integer
part1 = product . map numPossible

-- | The number of button press durations that go far enough. 
-- t is the time allowed for the race, and d is the record distance that must be matched or beaten
-- Use the quadratic formula to solve x * (t - x) > d.  Round away from the answer and then
-- add/subtract 1 to make it ">" and not ">=".
numPossible :: Race -> Integer
numPossible (t, d) =
    maxTime - minTime + 1
    where
        minTime = floor ((int2Double t - s4ac) / 2) + 1
        maxTime = ceiling ((int2Double t + s4ac) / 2) - 1
        s4ac = sqrt (int2Double (t * t - 4 * d))

part2 :: Problem -> Integer
part2 problem = 
    numPossible (t, d)
    where
        t = unkern times 
        d = unkern distances 
        (times, distances) = unzip problem
        unkern = read . filter (/= ' ') . unwords . map show

