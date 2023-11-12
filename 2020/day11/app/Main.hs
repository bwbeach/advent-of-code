{-# LANGUAGE InstanceSigs #-}

module Main where

import Advent (Grid (..), gridBounds, gridFormat, gridGet, gridMap, gridParse, run)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust, listToMaybe, mapMaybe)
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
  countOccupied . firstSame . iterate (lifeStep rule) . boundedGrid
  where
    firstSame = fst . head . filter (uncurry (==)) . pairs
    countOccupied = length . filter (== '#') . map snd . lgCells

part2 :: Problem -> Int
part2 =
  countOccupied . firstSame . iterate (lifeStepWithRay rule2) . boundedGrid
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

-- | Bounds of a rectangle: minX minY maxX maxY
data Bounds
  = Bounds Point Point
  deriving (Eq, Show)

allPoints :: Bounds -> [Point]
allPoints (Bounds (V2 x0 y0) (V2 x1 y1)) =
  [ V2 x y
    | x <- [x0 .. x1],
      y <- [y0 .. y1]
  ]

inBounds :: Bounds -> Point -> Bool
inBounds (Bounds (V2 x0 y0) (V2 x1 y1)) (V2 x y) =
  x0 <= x && x <= x1 && y0 <= y && y <= y1

data BoundedGrid
  = BoundedGrid Bounds Grid
  deriving (Eq, Show)

boundedGrid :: Grid -> BoundedGrid
boundedGrid g =
  BoundedGrid (Bounds minP maxP) g
  where
    (minP, maxP) = gridBounds g

boundedGridGet :: Point -> BoundedGrid -> Maybe Char
boundedGridGet p (BoundedGrid b g) =
  if inBounds b p
    then Just $ gridGet p g
    else Nothing

type Point = V2 Int

class LifeGrid g where
  lgNew :: g -> [(Point, Char)] -> g
  lgGet :: Point -> g -> Maybe Char
  lgCells :: g -> [(Point, Char)]
  lgCandidates :: g -> [Point]

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
  lgCandidates (BoundedGrid bounds _) = allPoints bounds

-- | Returns all of the unique values from the list
-- ... and happens to return them in order.
unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

firstVisible :: (LifeGrid g) => V2 Int -> V2 Int -> g -> Maybe Char
firstVisible p d bg =
  find (/= ' ') . map fromJust . takeWhile isJust . map (get bg) $ ray p d
  where
    get g p = lgGet p g

ray :: V2 Int -> V2 Int -> [V2 Int]
ray p d = iterate (+ d) (p + d)

-- | Computes the next step in a conway-life-style simulation.
lifeStep :: (LifeGrid g) => (Char -> [Char] -> Char) -> g -> g
lifeStep newCellState g =
  lgNew g result
  where
    result =
      [ (p, c)
        | p <- lgCandidates g,
          let c = newCellState (fromJust $ get p) (mapMaybe get (pointNeighbors p)),
          c /= ' '
      ]
    get p = lgGet p g

lifeStepWithRay :: (LifeGrid g, Show g) => (Char -> [Char] -> Char) -> g -> g
lifeStepWithRay newCellState g =
  lgNew g result
  where
    result =
      [ (p, c)
        | p <- lgCandidates g,
          let c = newCellState (fromJust $ get p) (mapMaybe (fv p) eightDirections),
          c /= ' '
      ]
    get p = lgGet p g
    fv start dir = firstVisible start dir g

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
