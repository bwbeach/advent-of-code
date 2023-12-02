module Main where

import Advent (run)
import Data.Char
import Data.List (tails)
import Data.List.Extra (replace, takeEnd)
import Data.Maybe (mapMaybe)

main :: IO ()
main = run parse part1 part2

type Problem = [String]

parse :: String -> Problem
parse = lines

part1 :: Problem -> Int
part1 = sum . map (calibration . filter isDigit)

calibration :: String -> Int
calibration digits =
  if null digits
    then 0
    else read (take 1 digits ++ takeEnd 1 digits)

part2 :: Problem -> Int
part2 = sum . map (calibration . findDigits)

-- | Replace all digit names, from left to right.
findDigits :: String -> String
findDigits =
  concat . mapMaybe getDigit . tails
  where
    getDigit :: String -> Maybe String
    gitDigit [] = Nothing
    getDigit (c : _) | isDigit c = Just [c]
    getDigit ('o' : 'n' : 'e' : _) = Just "1"
    getDigit ('t' : 'w' : 'o' : _) = Just "2"
    getDigit ('t' : 'h' : 'r' : 'e' : 'e' : _) = Just "3"
    getDigit ('f' : 'o' : 'u' : 'r' : _) = Just "4"
    getDigit ('f' : 'i' : 'v' : 'e' : _) = Just "5"
    getDigit ('s' : 'i' : 'x' : _) = Just "6"
    getDigit ('s' : 'e' : 'v' : 'e' : 'n' : _) = Just "7"
    getDigit ('e' : 'i' : 'g' : 'h' : 't' : _) = Just "8"
    getDigit ('n' : 'i' : 'n' : 'e' : _) = Just "9"
    getDigit _ = Nothing
