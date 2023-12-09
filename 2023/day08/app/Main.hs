module Main where

import Advent (run)
import Data.List.Extra (dropEnd, scanl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main = run parse part1 part2

type Problem = (String, M.Map String (String, String))

parse :: String -> Problem
parse s =
  (turns, mapping)
  where
    [turns, mapText] = splitOn "\n\n" s
    mapping = M.fromList . map parseOneMap . lines $ mapText

parseOneMap :: String -> (String, (String, String))
parseOneMap s =
  (from, (left, right))
  where
    [from, choices] = splitOn " = " s
    [left, right] = splitOn ", " . drop 1 . dropEnd 1 $ choices

part1 :: Problem -> Int
part1 (turns, mapping) =
  length . takeWhile (/= "ZZZ") . scanl' oneMove "AAA" . cycle $ turns
  where
    oneMove :: String -> Char -> String
    oneMove from turn = (if turn == 'L' then fst else snd) (mapping M.! from)

part2 :: Problem -> Int
part2 = length
