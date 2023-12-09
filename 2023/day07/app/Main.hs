module Main where

import Advent (run)
import Data.List (group, sort)
import Data.Tuple.Extra (first)

main :: IO ()
main = run parse part1 part2

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Show)

newtype Hand = Hand String deriving (Eq)

instance Read Hand where
  readsPrec _ s =
    [(Hand h, x)]
    where
      (h, x) = go . skipSpaces $ s
      go "" = ("", "")
      go (' ' : cs) = ("", cs)
      go (c : cs) = first (c :) (go cs)
      skipSpaces (' ' : cs) = cs
      skipSpaces cs = cs

instance Show Hand where
  show (Hand s) = s

handType :: Hand -> HandType
handType (Hand s) =
  case sort . map length . group . sort $ s of
    [1, 1, 1, 1, 1] -> HighCard
    [1, 1, 1, 2] -> OnePair
    [1, 2, 2] -> TwoPair
    [1, 1, 3] -> ThreeOfAKind
    [2, 3] -> FullHouse
    [1, 4] -> FourOfAKind
    [5] -> FiveOfAKind
    _ -> error ("bad hand in handType: " ++ s)

handRanks :: Hand -> [Int]
handRanks (Hand s) =
  map cardRank s
  where
    cardRank c =
      case c of
        'T' -> 10
        'J' -> 11
        'Q' -> 12
        'K' -> 13
        'A' -> 14
        _ -> read [c]

instance Ord Hand where
  compare h1 h2 =
    case compare (handType h1) (handType h2) of
      LT -> LT
      EQ -> compare (handRanks h1) (handRanks h2)
      GT -> GT

type Problem = [(Hand, Int)] -- each pair is a Hand and the corresponding bid

parse :: String -> Problem
parse =
  map parseLine . lines
  where
    parseLine s =
      (read a, read b)
      where
        [a, b] = words s

part1 :: Problem -> Int
part1 = sum . zipWith (*) [1 ..] . map snd . sort

part2 :: Problem -> Int
part2 = length
