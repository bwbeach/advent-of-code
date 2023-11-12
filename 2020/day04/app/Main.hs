module Main where

import Advent (run)
import Data.List.Extra (dropEnd, takeEnd)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Text.Regex.TDFA

main :: IO ()
main = run parse part1 part2

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
part2 =
  length . filter isValid
  where
    isValid :: M.Map String String -> Bool
    isValid m = all (oneValid m) required

    oneValid :: M.Map String String -> (String, String -> Bool) -> Bool
    oneValid m (f, p) = maybe False p (M.lookup f m)

    required :: [(String, String -> Bool)]
    required =
      [ ("byr", validByr),
        ("iyr", validIyr),
        ("eyr", validEyr),
        ("ecl", validEcl),
        ("hcl", validHcl),
        ("pid", validPid),
        ("hgt", validHgt)
      ]

    validByr = numberInRange 1920 2002
    validIyr = numberInRange 2010 2020
    validEyr = numberInRange 2020 2030
    validEcl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    validPid s = allDigits s && length s == 9

    validHcl :: String -> Bool
    validHcl s = s =~ "\\`#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\'"

    validHgt s =
      2 < length s && go s
      where
        go s =
          case units of
            "in" -> numberInRange 59 76 num
            "cm" -> numberInRange 150 193 num
            _ -> False
        num = dropEnd 2 s
        units = takeEnd 2 s

    numberInRange :: Int -> Int -> String -> Bool
    numberInRange low high s =
      allDigits s && low <= read s && read s <= high

    allDigits :: String -> Bool
    allDigits = (=~ "\\`[0-9]*\\'")
