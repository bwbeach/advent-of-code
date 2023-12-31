{-# LANGUAGE InstanceSigs #-}

module Main where

import Advent
  ( Grid (..),
    LifeGrid (lgCandidates, lgCells, lgGet, lgNew),
    Rectangle (..),
    gridBounds,
    gridFormat,
    gridGet,
    gridMap,
    gridParse,
    life,
    rectangleContains,
    rectanglePoints,
    run,
  )
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Linear.V2 (V2 (..))
import Topograph (G (gDiff), pairs)

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse . replace '.' ' '
  where
    replace a b = map (\x -> if x == a then b else x)

part1 :: Problem -> Int
part1 =
  countOccupied . firstSame . life (const pointNeighbors) rule . boundedGrid
  where
    firstSame = fst . head . filter (uncurry (==)) . pairs
    countOccupied = length . filter (== '#') . map snd . lgCells

part2 :: Problem -> Int
part2 =
  countOccupied . firstSame . life rayNeighbors rule2 . boundedGrid
  where
    firstSame = fst . head . filter (uncurry (==)) . pairs
    countOccupied = length . filter (== '#') . map snd . lgCells

rule :: Char -> [Char] -> Char
rule c ns =
  case c of
    'L' -> if occupied == 0 then '#' else 'L'
    '#' -> if occupied >= 4 then 'L' else '#'
    _ -> c
  where
    occupied = count '#' ns

rule2 :: Char -> [Char] -> Char
rule2 c ns =
  case c of
    'L' -> if occupied == 0 then '#' else 'L'
    '#' -> if occupied >= 5 then 'L' else '#'
    _ -> c
  where
    occupied = count '#' ns

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (== a)

data BoundedGrid
  = BoundedGrid Rectangle Grid
  deriving (Eq, Show)

boundedGrid :: Grid -> BoundedGrid
boundedGrid g =
  BoundedGrid (Rectangle minP maxP) g
  where
    (minP, maxP) = gridBounds g

boundedGridGet :: Point -> BoundedGrid -> Maybe Char
boundedGridGet p (BoundedGrid b g) =
  if rectangleContains b p
    then Just $ gridGet p g
    else Nothing

boundedGridInBounds :: Point -> BoundedGrid -> Bool
boundedGridInBounds p (BoundedGrid b _) = rectangleContains b p

type Point = V2 Int

instance LifeGrid Grid where
  lgNew :: Grid -> [(Point, Char)] -> Grid
  lgNew _ = Grid . M.fromList

  lgGet :: Point -> Grid -> Maybe Char
  lgGet p g = Just (gridGet p g)

  lgCells :: Grid -> [(Point, Char)]
  lgCells = M.toList . gridMap

  lgCandidates :: Grid -> [Point]
  lgCandidates = unique . concatMap pointAndNeighbors . M.keys . gridMap

instance LifeGrid BoundedGrid where
  lgNew :: BoundedGrid -> [(Point, Char)] -> BoundedGrid
  lgNew (BoundedGrid bounds _) items = BoundedGrid bounds (Grid . M.fromList $ items)

  lgGet :: Point -> BoundedGrid -> Maybe Char
  lgGet = boundedGridGet

  lgCells (BoundedGrid _ g) = M.toList . gridMap $ g

  lgCandidates :: BoundedGrid -> [Point]
  lgCandidates (BoundedGrid bounds _) = rectanglePoints bounds

-- | Returns all of the unique values from the list
-- ... and happens to return them in order.
unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

rayNeighbors :: (LifeGrid g) => g -> Point -> [Point]
rayNeighbors g p =
  mapMaybe firstVisiblePoint eightDirections
  where
    firstVisiblePoint = find nonEmpty . ray
    ray dir = takeWhile inBounds . iterate (+ dir) $ (p + dir)
    inBounds p = isJust $ lgGet p g
    nonEmpty p = (/= ' ') . fromMaybe ' ' $ lgGet p g

pointAndNeighbors :: Point -> [Point]
pointAndNeighbors p = p : pointNeighbors p

pointNeighbors :: Point -> [Point]
pointNeighbors p = map (+ p) eightDirections

eightDirections :: [Point]
eightDirections =
  [ V2 x y
    | x <- [-1 .. 1],
      y <- [-1 .. 1],
      x /= 0 || y /= 0
  ]
