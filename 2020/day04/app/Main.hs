module Main where

import Advent (runTestAndInput)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = runTestAndInput parse part1 part2

parse :: String -> [M.Map String String]
parse =
  map parsePassport . splitOn "\n\n"
  where
    parsePassport = M.fromList . map parseElem . words
    parseElem = pair . splitOn ":"
    pair [a, b] = (a, b)

part1 :: [M.Map String String] -> Int
part1 =
  length . filter isValid
  where
    isValid = (== required) . S.intersection required . S.fromList . M.keys
    required = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part2 :: [M.Map String String] -> Int
part2 _ = 5
