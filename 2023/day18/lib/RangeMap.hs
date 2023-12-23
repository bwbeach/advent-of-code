module RangeMap
  ( Range (..),
    RangeGrid,
    RangeMap,
    rangeSize,
    rgEmpty,
    rgFormat,
    rgInsert,
    rmEmpty,
    rmRangeEnds,
    rmToList,
  )
where

import Data.List (find, foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Type.Bool as RangeMaps

-- | A range of numbers
data Range a
  = Range a a
  deriving (Eq, Ord, Show)

-- | Number of things in a range
rangeSize :: (Integral k) => Range k -> k
rangeSize (Range a b) = b - a + 1

-- | Do two ranges overlap?
--
-- >>> rangeOverlaps (Range 4 9) (Range 1 5)
-- True
--
-- >>> rangeOverlaps (Range 1 4) (Range 5 9)
-- False
rangeOverlaps :: (Ord a) => Range a -> Range a -> Bool
rangeOverlaps (Range a b) (Range c d) = a <= d && c <= b

-- | Are two ranges disjoint?
rangeDisjoint :: (Ord a) => Range a -> Range a -> Bool
rangeDisjoint (Range a b) (Range c d) = b < c || d < a

-- | Is the first range adjacent to the second range?
rangeAdjacent :: (Integral a) => Range a -> Range a -> Bool
rangeAdjacent (Range _ b) (Range c _) = b + 1 == c

-- | Merge two adjacent ranges
joinRanges :: Range a -> Range a -> Range a
joinRanges (Range a b) (Range c d) = Range a d

-- | Does the first range completely contain the second range?
--
-- >>> rangeContains (Range 1 9) (Range 2 8)
-- True
rangeContains :: (Ord a) => Range a -> Range a -> Bool
rangeContains (Range a b) (Range c d) = a <= c && d <= b

-- | Is the first range completely before the second?
rangeBefore :: (Ord a) => Range a -> Range a -> Bool
rangeBefore (Range _ b) (Range c _) = b < c

-- | Union of two overlapping ranges
rangeUnion :: (Ord a, Show a) => Range a -> Range a -> Range a
rangeUnion r1@(Range a b) r2@(Range c d)
  | r1 `rangeOverlaps` r2 = Range (min a c) (max c d)
  | otherwise = error ("union of non-overlapping ranges: " ++ show r1 ++ " " ++ show r2)

-- | Add a range to a sorted list of ranges, combining overlaps.
-- Assumes the list has already had overlaps combined, so the only overlaps
-- to consider are with the new range.
--
-- >>> addRangeToList (Range 4 6) [Range 1 3, Range 5 8, Range 11 12]
-- [Range 1 3,Range 4 8,Range 11 12]
addRangeToList :: (Ord a, Show a) => Range a -> [Range a] -> [Range a]
addRangeToList a [] = [a]
addRangeToList a (r : rs)
  | a `rangeBefore` r = a : r : rs
  | a `rangeOverlaps` r = addRangeToList (rangeUnion a r) rs
  | otherwise = r : addRangeToList a rs

newtype RangeMap k v
  = RangeMap [(Range k, v)]
  deriving (Eq)

instance (Show k, Show v) => Show (RangeMap k v) where
  show (RangeMap items) = "rmFromList " ++ show items

-- | An empty map
--
-- >>> rmEmpty
-- rmFromList []
rmEmpty :: RangeMap k v
rmEmpty = RangeMap []

-- | Build an RangeMap from a list of (range, value) pairs.
rmFromList :: (Integral k, Eq v) => [(Range k, v)] -> RangeMap k v
rmFromList =
  foldl' add rmEmpty
  where
    add rm (k, v) = rmInsert k v rm

-- | Returns the list of (range, value) pairs in a map.
rmToList :: RangeMap k v -> [(Range k, v)]
rmToList (RangeMap kvs) = kvs

-- | Returns the value for a given k
rmGet :: (Ord k) => k -> RangeMap k v -> Maybe v
rmGet k (RangeMap kvs) =
  fmap snd . find keyMatches $ kvs
  where
    keyMatches (Range a b, v) = a <= k && k <= b

-- | Return the ends of all the ranges in the map.
--
-- >>> rmRangeEnds (RangeMap [(Range 1 4, 'A'), (Range 5 6, 'B')])
-- [1,4,5,6]
rmRangeEnds :: RangeMap k v -> [k]
rmRangeEnds (RangeMap kvs) =
  concatMap (rangeEnds . fst) kvs
  where
    rangeEnds (Range a b) = [a, b]

-- | All of the values in a RangeMap
--
-- >>> rmValues (RangeMap [(Range 1 4, 100), (Range 5 6, 200)])
-- [100,200]
rmValues :: RangeMap k v -> [v]
rmValues (RangeMap kvs) = map snd kvs

-- | Constructor for lists of range/value pairs.
-- Helper function for rmUpdate.  Normalizes lists of range/value pairs.
-- Combines adjacent pairs that have the same value, and drops pairs with Nothing.
-- Assumes that the second argument, `kvs`, has already
--
-- >>> rmCons (Range 2 3, Just 'A') [(Range 4 5, 'A')]
-- [(Range 2 5,'A')]
--
-- >>> rmCons (Range 2 3, Just 'A') [(Range 4 5, 'B')]
-- [(Range 2 3,'A'),(Range 4 5,'B')]
--
-- >>> rmCons (Range 2 3, Nothing) [(Range 4 5, 'B')]
-- [(Range 4 5,'B')]
rmCons :: (Integral k, Eq v) => (Range k, Maybe v) -> [(Range k, v)] -> [(Range k, v)]
rmCons (_, Nothing) kvs = kvs
rmCons (k, Just v) [] = [(k, v)]
rmCons (k, Just v) ((k0, v0) : kvs) =
  if rangeAdjacent k k0 && v == v0
    then (joinRanges k k0, v) : kvs
    else (k, v) : (k0, v0) : kvs

-- | Update a value for a range of keys by appling a function to the existing value(s).
rmUpdate :: (Integral k, Eq v) => Range k -> (Maybe v -> Maybe v) -> RangeMap k v -> RangeMap k v
rmUpdate k f (RangeMap oldKvs) =
  RangeMap (go k f oldKvs)
  where
    -- go :: Range k -> (Maybe v -> Maybe v) -> [(k, v)] -> [(k, v)]
    go k f [] = rmCons (k, f Nothing) []
    go k@(Range a b) f (kv0@(k0@(Range c d), v0) : kvs)
      -- Skip over ranges before the one being added
      | d < a = (Range c d, Just v0) `rmCons` go k f kvs
      -- New range completely before the existing one
      | b < c = (k, f Nothing) `rmCons` (kv0 : kvs)
      -- Break off the part of the existing range before the one being added
      | c < a = (Range c (a - 1), Just v0) `rmCons` go k f ((Range a d, v0) : kvs)
      -- Break off the part of the new range before the existing one
      | a < c = (Range a (c - 1), f Nothing) `rmCons` go (Range c b) f (kv0 : kvs)
      -- New range matches existing one
      | k == k0 = (k, f (Just v0)) `rmCons` kvs
      -- New range shorter than existing one
      | b < d = (k, f (Just v0)) `rmCons` ((Range (b + 1) d, v0) : kvs)
      -- New range is longer than existing one
      | otherwise = (k0, f (Just v0)) `rmCons` go (Range (d + 1) b) f kvs

-- | Insert a value for a range of keys.
--
-- >>> rmInsert (Range 4 6) 'B' rmEmpty
-- rmFromList [(Range 4 6,'B')]
--
-- >>> rmInsert (Range 1 2) 'A' (rmFromList [(Range 4 6, 'B')])
-- rmFromList [(Range 1 2,'A'),(Range 4 6,'B')]
--
-- >>> rmInsert (Range 1 6) 'A' (rmFromList [(Range 4 5, 'B')])
-- rmFromList [(Range 1 6,'A')]
--
-- >>> rmInsert (Range 4 6) 'B' (rmFromList [(Range 1 9, 'A')])
-- rmFromList [(Range 1 3,'A'),(Range 4 6,'B'),(Range 7 9,'A')]
rmInsert :: (Integral k, Eq v) => Range k -> v -> RangeMap k v -> RangeMap k v
rmInsert k v = rmUpdate k (const (Just v))

-- | A RangeGrid is a grid represented as a RangeMap of RangeMaps.
type RangeGrid k v = RangeMap k (RangeMap k v)

-- | An empty RangeGrid
rgEmpty :: RangeGrid k v
rgEmpty = rmEmpty

-- | Updates the value for all points in a rectangle.
rgUpdate :: (Integral k, Eq v) => Range k -> Range k -> (Maybe v -> Maybe v) -> RangeGrid k v -> RangeGrid k v
rgUpdate xRange yRange f =
  rmUpdate xRange updateOneSlice
  where
    updateOneSlice rm = Just (rmUpdate yRange f (fromMaybe rmEmpty rm))

-- | Sets the value for all points in a rectangle
--
-- >>> rgInsert (Range 15 25) (Range 5 8) 'B' rgEmpty
-- rmFromList [(Range 15 25,rmFromList [(Range 5 8,'B')])]
--
-- >>> rgInsert (Range 15 25) (Range 5 8) 'B' . rgInsert (Range 10 20) (Range 4 6) 'A' $ rgEmpty
-- rmFromList [(Range 10 14,rmFromList [(Range 4 6,'A')]),(Range 15 20,rmFromList [(Range 4 4,'A'),(Range 5 8,'B')]),(Range 21 25,rmFromList [(Range 5 8,'B')])]
rgInsert :: (Integral k, Eq v) => Range k -> Range k -> v -> RangeGrid k v -> RangeGrid k v
rgInsert xRange yRange v = rgUpdate xRange yRange (const (Just v))

-- | Formats a RangeGrid for printing.
-- The result is not to scale -- every range of values possible is reduced to one cell wide.
--
-- >>> rgFormat (rgInsert (Range 15 25) (Range 5 8) 'B' rgEmpty) id
-- "BB\nBB\n"
--
-- >>> rgFormat (rgInsert (Range 5 5) (Range 5 5) 'A' rgEmpty) id
-- "A\n"
rgFormat :: (Ord k) => RangeGrid k v -> (v -> Char) -> String
rgFormat rg vToChar =
  unlines . map oneRow $ allYs
  where
    allXs = S.toList . S.fromList . rmRangeEnds $ rg
    allYs = S.toList . S.fromList . concatMap rmRangeEnds . rmValues $ rg
    oneRow y = map (oneCell y) allXs
    oneCell y x = maybe ' ' vToChar $ rgGet x y rg

-- | Returns the value at a given location in a RangeGrid, if there is one
rgGet :: (Ord k) => k -> k -> RangeGrid k v -> Maybe v
rgGet x y = rmGet y . fromMaybe rmEmpty . rmGet x
