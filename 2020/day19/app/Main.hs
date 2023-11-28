module Main where

import Advent (run)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

import Debug.Trace

main :: IO ()
main = run parse part1 part2

data Elem
  = Literal Char
  | Nonterminal Int
  | Alternate [Elem]
  | Sequence [Elem]
  deriving (Eq, Ord, Show)

type Grammar = M.Map Int Elem

type Problem = (Grammar, [String])

parse :: String -> Problem
parse s =
  (grammar, lines codeText)
  where
    [grammarText, codeText] = splitOn "\n\n" s
    grammar = M.fromList (map parseRule . lines $ grammarText)

-- | Parse one rule in the grammar
--
-- >>> parseRule "0: 1 2 | 3 4"
-- (0,Alternate [Sequence [Nonterminal 1,Nonterminal 2],Sequence [Nonterminal 3,Nonterminal 4]])
--
-- >>> parseRule "4: \"a\""
-- (4,Literal 'a')
parseRule :: String -> (Int, Elem)
parseRule s =
  (read n, e)
  where
    [n, rhs] = splitOn ": " s
    e =
      if head rhs == '"'
        then Literal (rhs !! 1)
        else Alternate . map parseNtSequence . splitOn "|" $ rhs
    parseNtSequence = Sequence . map (Nonterminal . read) . words

part1 :: Problem -> Int
part1 (grammar, codes) = length . filter (matches grammar [Nonterminal 0]) $ codes

part2 :: Problem -> Int
part2 = length . snd

matches :: Grammar -> [Elem] -> String -> Bool
matches _ [] s = null s
matches _ _ [] = False
matches g (e : es) s =
  case e of
    Literal c -> c == head s && matches g es (tail s)
    Sequence es' -> matches g (es' ++ es) s
    Alternate es' -> any (\a -> matches g (a : es) s) es'
    Nonterminal n -> matches g ((g M.! n) : es) s
