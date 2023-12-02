module Main where

import Advent (run)
import Data.List.Split (splitOn)

main :: IO ()
main = run parse part1 part2

type Color = String 

type Draw = [(Color, Int)]

type Game = (Int, [Draw])

type Problem = [Game]

parse :: String -> Problem
parse = map parseLine . lines 

-- | Parses one input line. 
--
-- Example:
-- Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
--
parseLine :: String -> Game 
parseLine s =
    (gameNumber, draws)
    where 
        [lhs, rhs] = splitOn ": " s 
        gameNumber = read . drop 5 $ lhs 
        draws = map parseDraw . splitOn "; " $ rhs 
        parseDraw :: String -> Draw
        parseDraw = map parseColor . splitOn ", "
        parseColor s = 
            (b, read a)
            where 
                [a, b] = words s

part1 :: Problem -> Int
part1 = 
    sum . map fst . filter (isPossible . snd) 
    where 
        isPossible = all (all colorPossible)
        colorPossible ("red", n) = n <= 12
        colorPossible ("green", n) = n <= 13
        colorPossible ("blue", n) = n <= 14

part2 :: Problem -> Int
part2 = length
