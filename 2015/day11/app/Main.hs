module Main where

import Advent (run)
import Data.Char
import Data.Foldable
import Data.List

main :: IO ()
main = run parse part1 part2

type Problem = String

parse :: String -> Problem
parse = filter isAlpha

part1 :: Problem -> String
part1 = nextPassword

part2 :: Problem -> String
part2 = nextPassword . nextPassword

-- | What's the next valid password?
--
-- >>> nextPassword "abcdefgh"
-- "abcdffaa"
--
-- >>> nextPassword "ghijklmn"
-- "ghjaabcc"
nextPassword :: String -> String
nextPassword = head . filter isValid . drop 1 . iterate incrementPassword

-- | Increment a password by adding one to the last letter and carrying.
--
-- >>> incrementPassword "xyz"
-- "xza"
incrementPassword :: String -> String
incrementPassword =
  snd . foldr' oneStep (1, "")
  where
    oneStep c (carry, soFar) =
      (carry', c' : soFar)
      where
        (c', carry') = addToLetterWithCarry c carry

-- | Increment a char, returning new char and int carry.
--
-- >>> addToLetterWithCarry 'a' 1
-- ('b',0)
--
-- >>> addToLetterWithCarry 'z' 1
-- ('a',1)
addToLetterWithCarry :: Char -> Int -> (Char, Int)
addToLetterWithCarry c d =
  (toLetter (n' `mod` 26), n' `div` 26)
  where
    n' = fromLetter c + d

-- | Convert a lowercase letter to a mod-26 number
--
-- >>> fromLetter 'd'
-- 3
fromLetter :: Char -> Int
fromLetter c = ord c - ord 'a'

-- | Convert a mod-26 number to a lowercase letter.
--
-- >>> toLetter 3
-- 'd'
toLetter :: Int -> Char
toLetter n = chr (n + ord 'a')

-- | Is a password valid?
--
-- >>> isValid "abcdffaa"
-- True
--
-- >>> isValid "hijklmmn"
-- False
--
-- >>> isValid "abbceffg"
-- False
--
-- >>> isValid "abbcegjk"
-- False
isValid :: String -> Bool
isValid pw =
  noBadChar pw && hasRun pw && hasPairs 2 pw
  where
    noBadChar s = not $ any (`elem` s) "iol"

    hasRun = any startsWithRun . tails

    startsWithRun (a : b : c : _) =
      a `before` b && b `before` c
      where
        before x y = fromLetter x + 1 == fromLetter y
    startsWithRun _ = False

    hasPairs :: Int -> String -> Bool
    hasPairs 0 _ = True
    hasPairs n (a : b : cs) | a == b = hasPairs (n - 1) cs
    hasPairs n (_ : bs) = hasPairs n bs
    hasPairs _ _ = False
