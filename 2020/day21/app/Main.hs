{-# LANGUAGE TupleSections #-}

module Main where

import Advent (only, run)
import Data.List (find, foldl', intercalate, sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Tuple.Extra (second)

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
    allergenToPossibles = mtmFromList . concatMap aToP $ problem

part2 :: Problem -> String
part2 = intercalate "," . map snd . sort . findAllergens

-- | Finds the mapping from allergen to ingredient
-- Starts with a mapping allergenToSet (of possible ingredients).  Finds
-- an allergen with only one possible ingredient, then removes that allergen
-- and ingredient from the mapping, and repeats until all are done.
findAllergens :: Problem -> [(String, String)]
findAllergens problem =
  go allergenToSet
  where
    -- find the mappings among the things that haven't been mapped yet
    go aToI
      | M.null aToI = []
      | otherwise = (a, i) : go (removeAllergen a . removeIngredient i $ aToI)
      where
        (a, i) = second (only . S.toList) . fromJust . find ((== 1) . S.size . snd) . M.toList $ aToI
    -- remove an allergen from the alergen-to-ingredients mapping
    removeAllergen = M.delete
    -- remove an ingredient from the alergen-to-ingredients mapping
    removeIngredient i' = M.map (S.delete i')
    -- the initial, full mapping to search
    allergenToSet = M.map possibleSet . mtmFromList . concatMap aToP $ problem

-- | Extract (allergen, PossibileIngredients) pairs from one line the problem statement
aToP :: ProblemElem -> [(String, PossibleIngredients)]
aToP (ingredients, allergens) =
  (,possible) <$> allergens
  where
    -- the PossibleIngredients on the lhs of the line
    possible = OneOf . S.fromList $ ingredients

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
-- "mtm" is "map to monoid"
mtmFromList :: (Ord k, Monoid v) => [(k, v)] -> M.Map k v
mtmFromList =
  foldl' addOne M.empty
  where
    addOne m (k, v) = M.insert k (get k m <> v) m
    get = M.findWithDefault mempty
