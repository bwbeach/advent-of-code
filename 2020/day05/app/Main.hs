module Main where

import Advent (runTestAndInput)
import Data.List.Split
import qualified Data.Set as S

main :: IO ()
main = runTestAndInput parse part1 part2

parse :: String -> [String]
parse = endBy "\n"

part1 :: [String] -> Int
part1 = maximum . map seatId

-- | Finds a seat ID that's not present where the prev an next IDs are taken
part2 :: [String] -> [Int]
part2 passes =
  filter isTheOne [minimum seatIds .. maximum seatIds]
  where
    seatIds = map seatId passes
    seatIdSet = S.fromList seatIds
    isTheOne i = S.member (i - 1) seatIdSet && not (S.member i seatIdSet) && S.member (i + 1) seatIdSet

-- | Converts a boarding pass code to a Seat ID
-- The codes are binary once the characters are converted to ones and zeros.
seatId :: String -> Int
seatId =
  foldl go 0
  where
    go n c = n * 2 + charVal c
    charVal c = if c `elem` "RB" then 1 else 0
