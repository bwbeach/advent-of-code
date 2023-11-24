{-# LANGUAGE InstanceSigs #-}

module Main where

import Advent
  ( Point,
    gridMap,
    gridParse,
    run,
  )
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Tuple.Extra (first)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

main :: IO ()
main = run parse part1 part2

-- | A location in a Conway-style simulation.
class (Ord p) => ConwayPoint p where
  -- | The other points that are the neighborhood of a point.
  -- Includes the input point!  It's required that:
  --    p `elem` neighborhood p
  neighborhood :: p -> [p]

-- | Run a Conway-style simulation, returning the sequence of states.
-- The result starts with the initial state.
--
-- Each state is the set of locations that are active/alive.
conway :: (ConwayPoint p) => (Bool -> Int -> Bool) -> S.Set p -> [S.Set p]
conway rule =
  iterate step
  where
    -- \| Given a state, return the next state in the sequence.
    step s = S.fromList . filter (passesRule s) . candidates $ s
    -- \| What are all of the points that *might* be active in the next iteration?
    -- These are the points that are currently active, plus all of their neighbors.
    -- This assumes that: rule False 0 == False
    -- Otherwise, all empty cells without neighbors would become alive.\
    candidates = unique . concatMap neighborhood . S.toList
    -- \| All of the unique items in a list of items.
    unique = S.toList . S.fromList
    -- \| Given a state, and a location, is that location active in the next iteration?
    passesRule s p =
      rule (p `elem` s) (activeNeighbors p)
      where
        activeNeighbors = length . filter (`elem` s) . filter (/= p) . neighborhood

newtype PointN = PointN [Int] deriving (Eq, Ord, Show)

instance ConwayPoint PointN where
  neighborhood :: PointN -> [PointN]
  neighborhood (PointN as) =
    map PointN (go as)
    where
      go [] = [[]]
      go (n : ns) =
        [ n + d : ns'
          | ns' <- go ns,
            d <- [-1, 0, 1]
        ]

point3From2 :: Point -> PointN
point3From2 (V2 x y) = PointN [x, y, 0]

point4From2 :: Point -> PointN
point4From2 (V2 x y) = PointN [x, y, 0, 0]

type Problem = [Point]

parse :: String -> Problem
parse =
  map fst . M.toList . gridMap . gridParse . replace '.' ' '
  where
    replace a b = map (\x -> if x == a then b else x)

part1 :: Problem -> Int
part1 =
  length . S.toList . (!! 6) . conway rule . S.fromList . map point3From2

rule :: (Eq a, Num a) => Bool -> a -> Bool
rule False n = n == 3
rule True n = n == 2 || n == 3

part2 :: Problem -> Int
part2 =
  length . S.toList . (!! 6) . conway rule . S.fromList . map point4From2
