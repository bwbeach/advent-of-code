module Main where

import Advent (run)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Debug.Trace

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
  scoreHands . last . iterateMaybe oneRound

-- | Return the score for a pair of hands
scoreHands :: (Hand, Hand) -> Int
scoreHands (a, b) =
  if null a then scoreHand b else scoreHand a

scoreHand :: Hand -> Int
scoreHand = sum . zipWith (*) [1 ..] . reverse

-- | The hand that a player is holding.  First card in the list is the top card
type Hand = [Int]

-- | Who are the players
data Player = Player1 | Player2 deriving (Show)

-- | The state of a recursive game
data RecState = RecState
  { hands :: (Hand, Hand),
    prevHands :: S.Set (Hand, Hand)
  }
  deriving (Show)

-- | Who is the winner?
recWinner :: RecState -> Maybe Player
recWinner rs
  | S.member (hands rs) (prevHands rs) = trace "REPEAT" (Just Player1)
  | null (fst (hands rs)) = Just Player2
  | null (snd (hands rs)) = Just Player1
  | otherwise = Nothing

-- | Play a recursive game
recPlay :: RecState -> (Player, Hand)
recPlay rs =
  case recWinner rs of
    Nothing -> recPlay (recNextState rs)
    Just Player1 -> (Player1, fst (hands rs))
    Just Player2 -> (Player2, snd (hands rs))

-- | Next state in a recursive game
recNextState :: RecState -> RecState
recNextState rs@(RecState {hands = hs@(c1 : c1s, c2 : c2s), prevHands = phs}) =
  case roundWinner of
    Player1 -> rs {hands = (c1s ++ [c1, c2], c2s), prevHands = S.insert hs phs}
    Player2 -> rs {hands = (c1s, c2s ++ [c2, c1]), prevHands = S.insert hs phs}
  where
    -- Who wins this round?
    roundWinner
      | c1 <= length c1s && c2 <= length c2s = fst . recPlay $ subStart
      | c1 < c2 = Player2
      | c2 < c1 = Player1
      | otherwise = error "cards should not tie"
    -- Starting point for a recursive game
    subStart = RecState {hands = (take c1 c1s, take c2 c2s), prevHands = S.empty}
recNextState _ = error "should not have tried recNextState when there was a winner"

part2 :: Problem -> Int
part2 problem =
  scoreHand . snd . recPlay $ RecState {hands = problem, prevHands = S.empty}
