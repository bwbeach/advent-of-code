module Main where

import Advent (run)
import Data.Char
import Data.List.Extra (takeEnd)

main :: IO ()
main = run parse part1 part2

type Problem = [String]

parse :: String -> Problem
parse = lines

part1 :: Problem -> Int
part1 = 
    sum . map calibration 
    where 
        calibration :: String -> Int
        calibration s = 
            read (take 1 digits ++ takeEnd 1 digits)
            where 
                digits = filter isDigit s  

part2 :: Problem -> Int
part2 = length
