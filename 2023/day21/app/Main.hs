{-# LANGUAGE TupleSections #-}

module Main where

import Advent
  ( Grid (..),
    Point,
    gridBounds,
    gridFormat,
    gridFromList,
    gridGet,
    gridMap,
    gridParse,
    gridToList,
    only,
    run,
  )
import Control.Exception (assert)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Tuple.Extra
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
  useQuadrants (double problem) start 26501365
  where
    -- Doubles the size of a tile, guaranteeing that it's even
    double = gridFromList . concatMap doubleEntry . gridToList
    doubleEntry (V2 x y, a) =
      [ (V2 x' y', a)
        | x' <- [x, x + tileSize],
          y' <- [y, y + tileSize]
      ]

    -- The start point
    start = findS problem

    -- The size of the (square tile)
    tileSize = getTileSize problem

-- | Return the size of an input tile.
-- Assumes that the tile is square, and the size applies in both directions.
getTileSize :: Grid -> Int
getTileSize grid =
  assert isSquare (x1 - x0 + 1)
  where
    isSquare = x1 - x0 == y1 - y0
    (V2 x0 y0, V2 x1 y1) = gridBounds grid

part2_test :: Problem -> Int
part2_test problem =
  length . filter (uncurry (/=) . traceShowId) . map ((algo1 &&& algo2) . traceShowId) $ [0 .. 500]
  where
    -- First algorithm: naive implementation
    algo1 n = snd $ runTile bigProblem start (n + 1)
    bigProblem = infiniteGrid problem

    -- Second algorithm: quandrant scores, which requires an even-sized tile
    algo2 = useQuadrants (double problem) start

    -- Doubles the size of a tile, guaranteeing that it's even
    double = gridFromList . concatMap doubleEntry . gridToList
    doubleEntry (V2 x y, a) =
      [ (V2 x' y', a)
        | x' <- [x, x + tileSize],
          y' <- [y, y + tileSize]
      ]

    -- The start point
    start = findS problem

    tileSize = getTileSize problem

useQuadrants :: Grid -> Point -> Int -> Int
useQuadrants problem start n =
  snd (runTile problem start (min (n + 1) (2 * tileSize + (n + 1) `mod` 2))) + (sum . map doOneQuadrant $ fourQuadrants)
  where
    doOneQuadrant (t, V2 xs ys) =
      quadrantScore t (V2 1 ys) axisSteps cornerSteps
      where
        axisSteps = n - (tileSize - xs)
        cornerSteps = axisSteps - (tileSize - ys + 1)

    -- The four quadrants
    fourQuadrants :: [(Grid, Point)]
    fourQuadrants = take 4 . iterate rotate $ (problem, start)
    -- map (first traceQuadrant) .

    traceQuadrant g =
      trace ("quadrant:\n" ++ gridFormat g ++ "\n\n") g

    -- Rotate the (tile, start)
    rotate (t, s) = (rotateTile t, rotatePoint s)

    -- Rotate the a tile
    rotateTile = gridFromList . map (first rotatePoint) . gridToList

    -- Rotate one point on the tile
    rotatePoint (V2 x y) = V2 (tileSize - y + 1) x

    tileSize = getTileSize problem

-- | What's the score in one quadrant?
quadrantScore ::
  Grid -> -- the tile
  Point -> -- the starting point, at the left side of the axis tile
  Int -> -- number of steps along the axis beyond the starting tile
  Int -> -- number of steps into the triangle beyond the starting tile
  Int -- total score for the quadrant
quadrantScore grid start stepsOnAxis stepsInTriangle =
  axisScore + triangleScore
  where
    axisScore = sum . map (uncurry (*)) $ axis
    triangleScore = sum . map (uncurry (*)) $ triangle
    axis = tilesInRow grid start stepsOnAxis
    triangle = triangleify $ tilesInRow grid (V2 1 1) stepsInTriangle

-- | Given a sequence of (count, item) for a row, determine counts for a triangle of them.
--
-- The input [(3, 'A'), (1, 'C'), (1, 'D')] represents the row AAACD
--
-- If that's the bottom row of the upper right quandrant, the full quadrant looks like this:
--
--     D
--     CD
--     ACD
--     AACD
--     AAACD
--
-- So the answer from this function is: [(6, 'A'), (4, 'C'), (5, 'D'))
--
-- >>> triangleify [(3, 'A'), (1, 'C'), (1, 'D')]
-- [(6,'A'),(4,'C'),(5,'D')]
triangleify :: (Show a) => [(Int, a)] -> [(Int, a)]
triangleify =
  go 0
  where
    -- given the number of items already processed, do the rest
    go _ [] = []
    go b ((n, a) : nas) = (triangleArea (b + n) - triangleArea b, a) : go (b + n) nas

    -- area of an triangle with base of length n
    triangleArea n = n * (n + 1) `div` 2

-- | Information about one type of tile
-- The pair holds:
--    - How many of this kind of tile there are
--    - The score (number of occupied locations)
type TileCountAndScore = (Int, Int)

-- | An 6x6 grid to use in examples
--
--    ......
--    ...#..
--    ......
--    ..##..
--    ..#...
--    ......
sampleTile :: Grid
sampleTile = gridParse "......\n...#..\n.S....\n..##..\n..#...\n......\n"

-- | TileCountAndScore for all of the tiles going right on the X axis.
--
-- Because we assume a clear path to the right from the start point, the
-- start point on the next tile will be the same.
-- >>> tilesInRow sampleTile (V2 1 3) 1
-- [(1,1)]
--
-- >>> tilesInRow sampleTile (V2 1 3) 13
-- [(1,15),(1,14),(1,1)]
--
-- >>> tilesInRow sampleTile (V2 1 3) 19
-- [(2,15),(1,14),(1,1)]
tilesInRow ::
  Grid -> -- One tile
  Point -> -- The starting point on that tile (always on the left edge)
  Int -> -- How many steps, after leaving the start tile
  [TileCountAndScore] -- All of the tiles the steps will visit along the X axis
tilesInRow g p n =
  reverse $ go n 0
  where
    -- Figure out the list of tiles, starting with the last one, given
    -- the number of steps up to starting this tile, and the number of steps
    -- to be taken after crossing this tile.
    go steps stepsAfter
      | steps <= 0 = []
      | hasConverged = assert (steps `mod` tileSize == 0) [(steps `div` tileSize, score)]
      | otherwise = (1, score) : go (steps - stepsInThisTile) (stepsInThisTile + stepsAfter)
      where
        (hasConverged, score) = runTile g p (stepsInThisTile + stepsAfter)
        stepsInThisTile = if steps `mod` tileSize == 0 then tileSize else steps `mod` tileSize

    tileSize = getTileSize g

-- | Run n steps in a tile, return (hasConverged, score)
--
-- An answer hasConverged iff every higher step count with the same
-- parity will give the same answer.
--
-- >>> runTile sampleTile (V2 1 3) 1
-- (False,1)
--
-- >>> runTile sampleTile (V2 1 3) 4
-- (False,8)
--
-- >>> runTile sampleTile (V2 1 3) 8
-- (False,17)
--
-- >>> runTile sampleTile (V2 1 3) 10
-- (True,17)
runTile :: (GenericGrid g) => g -> Point -> Int -> (Bool, Int)
runTile g p n =
  (hasConverged, score)
  where
    -- We've converged on a steady state (module parity) if no new locations were added
    hasConverged = S.null justAdded

    -- The n-th step into the tile
    (justAdded, _, score, _) = steps !! n

    -- the sequence of steps, starting with 0 steps to make indexing easier
    steps = (S.empty, S.empty, 0, 0) : allPossible2 g (S.singleton p, S.empty, 1, 0)

    traceSteps :: [(S.Set Point, x, y, z)] -> v -> v
    traceSteps steps =
      traceSet2 g combined
      where
        combined = foldl' S.union S.empty $ map getPoints steps
        getPoints (s, _, _, _) = s

traceSet2 :: (GenericGrid g) => g -> S.Set Point -> a -> a
traceSet2 g s =
  if S.null s
    then trace "empty\n\n"
    else trace (pretty ++ "\n\n")
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

traceSimpleSetId :: Char -> S.Set Point -> S.Set Point
traceSimpleSetId c s =
  trace pretty s
  where
    pretty = gridFormat . Grid . M.fromList $ pairsWithC
    pairsWithC = map (,c) . S.toList $ s

-- | A grid of cells on an integer plane, holding Chars.
class GenericGrid g where
  ggGet :: Point -> g -> Char

instance GenericGrid Grid where
  ggGet = gridGet

-- | An infinite grid creating by tiling a finite grid
data InfiniteGrid
  = InfiniteGrid (Int -> Int) (Int -> Int) Grid

-- | Create an infinite grid
--
-- >>> ggGet (V2 1 1) (infiniteGrid $ gridParse "ABC\nDEF\n")
-- 'A'
--
-- >>> ggGet (V2 1 4) (infiniteGrid $ gridParse "ABC\nDEF\n")
-- 'D'
--
-- >>> ggGet (V2 0 0) (infiniteGrid $ gridParse "ABC\nDEF\n")
-- 'F'
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
    width = x1 - x0 + 1
    height = y1 - y0 + 1

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
findS = only . map fst . filter ((== 'S') . snd) . gridToList
