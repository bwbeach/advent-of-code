module Main where

import Advent (run)
import Data.List.Split (splitOn)

main :: IO ()
main = run parse part1 part2

type Problem = ([Int], [Int])

-- | Parse the input file
--
-- >>> parse "Player 1:\n1\n2\n\nPlayer 2:\n3\n4\n"
-- ([1,2],[3,4])
parse :: String -> Problem
parse s =
  (parsePlayer a, parsePlayer b)
  where
    (a, b) = listToPair . splitOn "\n\n" $ s
    parsePlayer = map read . drop 1 . lines

-- | Convert a list of two things to a pair
listToPair :: (Show a) => [a] -> (a, a)
listToPair [a, b] = (a, b)
listToPair x = error ("List does not have exactly two: " ++ show x)

-- | One round of play
oneRound :: Problem -> Maybe Problem
oneRound (a : as, b : bs) =
  if a < b
    then Just (as, bs ++ [b, a])
    else Just (as ++ [a, b], bs)
oneRound _ = Nothing

-- | Like `iterate`, but stops when the function returns Nothing
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a =
  a : go a
  where
    go b =
      case f b of
        Nothing -> []
        Just c -> c : go c

part1 :: Problem -> Int
part1 =
  score . last . iterateMaybe oneRound
  where
    score (a, b) = if null a then scoreHand b else scoreHand a
    scoreHand = sum . zipWith (*) [1 ..] . reverse

part2 :: Problem -> Int
part2 = length
