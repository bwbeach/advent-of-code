{-# LANGUAGE TupleSections #-}

module Main where

import Advent
  ( Grid (..),
    Point,
    gridBounds,
    gridFormat,
    gridGet,
    gridMap,
    gridParse,
    only,
    run,
  )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Linear.V2 (V2 (..))
import Topograph (pairs)

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse

part1 :: Problem -> Int
part1 problem =
  total . (!! 64) $ allPossible2 problem (initial, S.empty, 1, 0)
  where
    -- The initial set of locations is the one place where the 'S' is
    initial = S.singleton . findS $ problem

    -- The total count from one tuple
    total (_, _, t, _) = t

part2 :: Problem -> Int
part2 problem =
  sum . map (S.size . traceSet bigProblem . newLocs) . take 30 $ allPossible2 bigProblem (initial, S.empty, 1, 0)
  where
    bigProblem = infiniteGrid problem

    -- The initial set of locations is the one place where the 'S' is
    initial = S.singleton . findS $ problem

    -- All odd-numbered elements of a list
    oddOnes (a : b : cs) = b : oddOnes cs

    -- The total count from one tuple
    total (_, _, t, _) = t

    -- The new locations added this iteration
    newLocs (locs, _, _, _) = locs

    -- Every nth item in a list
    every n items = head items : every n (drop (n - 1) items)

traceSet :: (GenericGrid g) => g -> S.Set Point -> S.Set Point
traceSet g s =
  trace (pretty ++ "\n\n") s
  where
    pretty = gridFormat . Grid . M.fromList $ (pairsWithO ++ rocksInRange)
    gridOfOs = Grid . M.fromList $ pairsWithO
    pairsWithO = map (,'O') . S.toList $ s
    rocksInRange =
      [ (p, '#')
        | x <- [x0 .. x1],
          y <- [y0 .. y1],
          let p = V2 x y,
          ggGet p g == '#'
      ]
    (V2 x0 y0, V2 x1 y1) = gridBounds gridOfOs

-- | A grid of cells on an integer plane, holding Chars.
class GenericGrid g where
  ggGet :: Point -> g -> Char

instance GenericGrid Grid where
  ggGet = gridGet

-- | An infinite grid creating by tiling a finite grid
data InfiniteGrid
  = InfiniteGrid (Int -> Int) (Int -> Int) Grid

-- | Create an infinite grid
infiniteGrid :: Grid -> InfiniteGrid
infiniteGrid g =
  InfiniteGrid mapX mapY g
  where
    -- Function to take an x value and map it to an x in the finite grid
    mapX x = x0 + (x - x0) `mod` width
    -- Function to take a y value and map it to a y in the finite grid
    mapY y = y0 + (y - y0) `mod` height

    -- Size of the tiled grid
    (V2 x0 y0, V2 x1 y1) = gridBounds g
    width = x1 - x0
    height = y1 - y0

instance GenericGrid InfiniteGrid where
  ggGet (V2 x y) (InfiniteGrid mapX mapY g) = gridGet (V2 (mapX x) (mapY y)) g

-- | Sequence of sets containing all possible positions at one time
allPossible :: (GenericGrid g) => g -> S.Set Point -> [S.Set Point]
allPossible problem =
  iterate next
  where
    -- Given a set of Os, where are the Os in the next iteration?
    next = S.fromList . filter isGarden . candidates

    -- Candidate locations where Os might be
    candidates = concatMap neighbors . S.toList

    -- Neighbors of a point
    neighbors p = map (p +) directions

    -- Deltas to neighbors
    directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

    -- Is the given location a garden plot?
    isGarden p = ggGet p problem `elem` ".S"

-- | Sequence of states: (mostRecent, prior, mostRecentCount, priorCount)
-- `mostRecent` is the set of locations added in the last pass
-- `prior` is the set of locations added in the pass before that
-- `mostRecentCount` is the sum of sizes of `mostRecent` and all previous sets of the same parity.
-- `priorCount` is the sum of sizes of `prior` and all previous sets of the same parity.
allPossible2 :: (GenericGrid g) => g -> (S.Set Point, S.Set Point, Int, Int) -> [(S.Set Point, S.Set Point, Int, Int)]
allPossible2 problem =
  iterate next
  where
    -- What's the next tuple in the sequence?
    next (s1, s2, c1, c2) =
      (s0, s1, c2 + S.size s0, c1)
      where
        -- What locations get added in this round?
        s0 = S.fromList . filter isGarden . filter (not . (`S.member` s2)) . candidates $ s1

        -- Candidate locations next to the previous set
        candidates = concatMap neighbors . S.toList

        -- Neighbors of a point
        neighbors p = map (p +) directions

        -- Deltas to neighbors
        directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

        -- Is the given location a garden plot?
        isGarden p = ggGet p problem `elem` ".S"

-- | Sequence of differences between adjacent numbers in a sequence
deltas :: (Num a) => [a] -> [a]
deltas = map (uncurry . flip $ (-)) . pairs

-- | Coordinates of the 'S' on the grid
findS :: Grid -> Point
findS = only . map fst . filter ((== 'S') . snd) . M.toList . gridMap
