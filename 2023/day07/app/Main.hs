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

-- | One hand of cards
-- Represented as a string, one character per card.
-- Jokers are '?', so they won't be confused with Jacks.
newtype Hand
  = Hand String
  deriving (Eq)

-- | Reads one Hand 
-- Assumes that the Hand is non-space characters surrounded by spaces.
instance Read Hand where
  readsPrec _ s =
    [(Hand h, x)]
    where
      (h, x) = go . dropWhile (== ' ') $ s

      go "" = ("", "")
      go (' ' : cs) = ("", cs)
      go (c : cs) = first (c :) (go cs)

instance Show Hand where
  show (Hand s) = s

-- | Given a hand without jokers, what type of hand is it?
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

-- | For a hand with jokers, what's the best hand type it could be?
--
-- >>> bestHandType (Hand "32T3K")
-- OnePair
--
-- >>> bestHandType (Hand "KT??T")
-- FourOfAKind
bestHandType :: Hand -> HandType
bestHandType = maximum . map handType . allPossible

-- | In a Hand that may contain jokers, what are all of the possible non-joker hands it could be?
--
-- >>> allPossible (Hand "4J4")
-- [4J4]
--
-- >>> allPossible (Hand "4?4")
-- [424,434,444,454,464,474,484,494,4T4,4Q4,4K4,4A4]
allPossible :: Hand -> [Hand]
allPossible (Hand s) =
  map Hand . foldr cardPossibleWith [[]] $ s
  where
    cardPossibleWith :: Char -> [String] -> [String]
    cardPossibleWith c rest =
      [ c' : r
        | c' <- cardPossible c,
          r <- rest
      ]
    cardPossible :: Char -> [Char]
    cardPossible '?' = "23456789TQKA"
    cardPossible c = [c]

handRanks :: Hand -> [Int]
handRanks (Hand s) =
  map cardRank s
  where
    cardRank c =
      case c of
        '?' -> 1
        'T' -> 10
        'J' -> 11
        'Q' -> 12
        'K' -> 13
        'A' -> 14
        _ -> read [c]

-- | Ordering of Hands
--
-- >>> compare (Hand "7J345") (Hand "72335")
-- LT
instance Ord Hand where
  compare h1 h2 =
    case compare (bestHandType h1) (bestHandType h2) of
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
part2 = sum . zipWith (*) [1 ..] . map snd . sort . map (first jackToJoker)

-- | Convert the Jacks in a hand to Jokers
--
-- >>> jackToJoker (Hand "4J7")
-- 4?7
jackToJoker :: Hand -> Hand
jackToJoker (Hand s) = Hand (replace 'J' '?' s)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)
