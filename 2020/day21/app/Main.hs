{-# LANGUAGE TupleSections #-}

module Main where

import Advent (run)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = run parse part1 part2

-- | Each line of input translates into a pair: (ingredients, allergens)
type ProblemElem = ([String], [String])

-- | The problem is represented as a list of pairs: (ingredients, allergens)
type Problem = [ProblemElem]

-- | Parse one input line
--
-- >>> parseLine "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
-- (["mxmxvkd","kfcds","sqjhc","nhms"],["dairy","fish"])
parseLine :: String -> ([String], [String])
parseLine s =
  case parts of
    [lhs, rhs] -> (words lhs, words rhs)
    _ -> error ("bad input line: " ++ s)
  where
    parts = splitOn "(contains" . filter (not . (`elem` "),")) $ s

-- | Parse an input file
parse :: String -> Problem
parse = map parseLine . lines

-- | Count of ingredient mentions for ingredients known to not contain allergens
part1 :: Problem -> Int
part1 problem =
  length . filter (not . (`S.member` maybeAllergen)) $ mentions
  where
    -- all mentions of ingredients
    mentions = concatMap fst problem
    -- set of all ingredients that might contain allergens
    maybeAllergen = S.unions . map possibleSet . M.elems $ allergenToPossibles
    -- map from allergen to possible ingredients
    allergenToPossibles = mtmFromList (concatMap aToP problem)

-- for one problem element, (allergen, PossibleIngredients) pairs

-- | Extract (allergen, PossibileIngredients) pairs
aToP :: ProblemElem -> [(String, PossibleIngredients)]
aToP (ingredients, allergens) =
  (,possible) <$> allergens
  where
    possible = OneOf . S.fromList $ ingredients

part2 :: Problem -> Int
part2 = length

-- | A Monoid that is the set of ingredients that might contain an allergen
data PossibleIngredients
  = Anything -- we don't have any restrictions on what the ingredients might be
  | OneOf (S.Set String)
  deriving (Show)

instance Semigroup PossibleIngredients where
  Anything <> b = b
  a <> Anything = a
  OneOf a <> OneOf b = OneOf (S.intersection a b)

instance Monoid PossibleIngredients where
  mempty = Anything

possibleSet :: PossibleIngredients -> S.Set String
possibleSet Anything = error "ingredients could be anything"
possibleSet (OneOf s) = s

-- | Take a list of pairs and make a map, mappend-ing multiple values for the same key
mtmFromList :: (Ord k, Monoid v) => [(k, v)] -> M.Map k v
mtmFromList =
  foldl' addOne M.empty
  where
    addOne m (k, v) =
      M.insert k (v' <> v) m
      where
        v' = M.findWithDefault mempty k m
