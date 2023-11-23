module Main where

import Advent (only, run)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace

main :: IO ()
main = run parse part1 part2

-- | A range of numbers, inclusive
data Range = Range Int Int deriving (Show)

-- | True iff the number is in the range.
inRange :: Int -> Range -> Bool
inRange n (Range a b) = a <= n && n <= b

-- | True if the number is in ANY of the ranges.
inRanges :: Int -> [Range] -> Bool
inRanges n = any (inRange n)

type Ticket = [Int]

data Problem = Problem
  { ranges :: M.Map String [Range],
    mine :: Ticket,
    nearby :: [Ticket]
  }
  deriving (Show)

parse :: String -> Problem
parse s =
  Problem {ranges = rs, mine = m, nearby = ns}
  where
    [a, b, c] = splitOn "\n\n" s
    rs = M.fromList . map parseRange . lines $ a
    m = parseTicket . last . lines $ b
    ns = map parseTicket . drop 1 . lines $ c

parseRange :: String -> (String, [Range])
parseRange s =
  (name, rs)
  where
    [name, rhs] = splitOn ": " s
    rs = map parseOneRange . splitOn "or" $ rhs

parseOneRange :: String -> Range
parseOneRange s =
  Range (read a) (read b)
  where
    [a, b] = splitOn "-" s

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

part1 :: Problem -> Int
part1 problem =
  sum . filter notInAnyRange . concat . nearby $ problem
  where
    notInAnyRange n = not . any (inRange n) $ allRanges
    allRanges = concat . ranges $ problem

part2 :: Problem -> Int
part2 = length . ranges

{-
  MapToSet is like a Map, but holds a Set of values for each key.
-}

-- | MapToSet is like a Map, but holds a Set of values for each key.
type MapToSet k v = M.Map k (S.Set v)

-- | An empty MapToSet
mtsEmpty :: (Ord k, Ord v) => MapToSet k v
mtsEmpty = M.empty

-- | Inserts a key/value pair into a MapToSet, adding the value to the set for the key.
--
-- >>> mtsInsert 1 5 . mtsInsert 1 6 . mtsInsert 2 7 $ mtsEmpty
-- fromList [(1,fromList [5,6]),(2,fromList [7])]
mtsInsert :: (Ord k, Ord v) => k -> v -> MapToSet k v -> MapToSet k v
mtsInsert k v m =
  M.insert k newSet m
  where
    oldSet = M.findWithDefault S.empty k m
    newSet = S.insert v oldSet

-- | Creates a MapToSet from a list of (k, v) pairs.
--
-- >>> mtsFromList [(1, 5), (1, 6), (2, 7)]
-- fromList [(1,fromList [5,6]),(2,fromList [7])]
mtsFromList :: (Ord k, Ord v) => [(k, v)] -> MapToSet k v
mtsFromList =
  foldl' insert mtsEmpty
  where
    insert m (k, v) = mtsInsert k v m

-- | List of (k, v) pairs for all keys that have just one value.
--
-- >>> mtsSingletons $ mtsFromList [(1, 5), (1, 6), (2, 7)]
-- [(2,7)]
mtsSingletons :: (Ord k, Ord v, Show k, Show v) => MapToSet k v -> [(k, v)]
mtsSingletons m =
  [ (k, v)
    | (k, vs) <- M.toList m,
      S.size vs == 1,
      let v = only . S.toList $ vs
  ]

-- | Finds all of the (a, b) pairs that must go together.
-- A pair must go together iff either one of them has only one match that works.
--
-- >>> findMatches ['a', 'b', 'c'] [1, 2, 3] (\c n -> (c, n) `elem` [('a', 1), ('a', 2), ('b', 2), ('c', 1), ('c', 3)])
-- [('b',2),('c',3)]
findMatches ::
  (Ord a, Ord b, Show a, Show b) =>
  [a] -> -- Things on the left side
  [b] -> -- Things on the right side
  (a -> b -> Bool) -> -- Predicate for testing match of left thing with right thing
  [(a, b)] -- Matches where either side has only one possible choice
findMatches as bs matches =
  uniqueItems
    ( mtsSingletons aToBs
        ++ map swap (mtsSingletons bToAs)
    )
  where
    uniqueItems = S.toList . S.fromList

    aToBs = mtsFromList possible
    bToAs = mtsFromList (map swap possible)

    possible =
      [ (a, b)
        | a <- as,
          b <- bs,
          matches a b
      ]

-- | Removes all elements of the first list from the second list.
--
-- >>> removeAll [1, 3, 5] [2, 3, 4, 5, 6]
-- [2,4,6]
removeAll :: (Eq a) => [a] -> [a] -> [a]
removeAll as = filter (not . (`elem` as))

-- | Repeatedly applies findMatches until all items are paired up.
--
-- >>> solveMatches ['a', 'b', 'c'] [1, 2, 3] (\c n -> (c, n) `elem` [('a', 1), ('a', 2), ('b', 2), ('c', 1), ('c', 3)])
-- [('b',2),('c',3),('a',1)]
solveMatches ::
  (Ord a, Ord b, Show a, Show b) =>
  [a] -> -- Things on the left side
  [b] -> -- Things on the right side
  (a -> b -> Bool) -> -- Predicate for testing match of left thing with right thing
  [(a, b)] -- Matches where either side has only one possible choice
solveMatches [] [] _ = []
solveMatches (_ : _) [] _ = error "mismatched counts"
solveMatches [] (_ : _) _ = error "mismatched counts"
solveMatches as bs matches =
  if null firstBatch
    then error "no solution"
    else firstBatch ++ theRest
  where
    firstBatch = findMatches as bs matches
    as' = removeAll (map fst firstBatch) as
    bs' = removeAll (map snd firstBatch) bs
    theRest = solveMatches as' bs' matches
