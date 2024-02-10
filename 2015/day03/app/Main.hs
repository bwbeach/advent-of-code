module Main where

import Advent (run)
import qualified Data.Set as S
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem = [V2 Int]

parse :: String -> Problem
parse = map charToVec

charToVec :: Char -> V2 Int
charToVec '<' = V2 (-1) 0
charToVec '^' = V2 0 1
charToVec '>' = V2 1 0
charToVec 'v' = V2 0 (-1)
charToVec c = error ("unexpected direction: " ++ show c)

part1 :: Problem -> Int
part1 = S.size . S.fromList . houses

part2 :: Problem -> Int
part2 problem =
  S.size . S.fromList $ santaHouses problem ++ roboHouses problem
  where
    santaHouses = houses . everyOther
    roboHouses = houses . everyOther . tail

-- | What houses are visited given a set of instructions?
houses :: Problem -> [V2 Int]
houses = scanl (+) (V2 0 0)

-- | Every second item in a list, starting with the first item
--
-- >>> everyOther [1, 2, 3, 4]
-- [1,3]
everyOther :: [a] -> [a]
everyOther [] = []
everyOther [a] = [a]
everyOther (a : _ : cs) = a : everyOther cs
