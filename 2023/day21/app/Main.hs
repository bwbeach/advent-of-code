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
import Control.Exception (assert)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
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
    initial = S.singleton . traceShowId . findS $ problem

    -- The total count from one tuple
    total (_, _, t, _) = t

part2 :: Problem -> Int
part2 problem =
  sum . map (S.size . traceSet bigProblem . newLocs) . take 30 $ allPossible2 bigProblem (initial, S.empty, 1, 0)
  where
    bigProblem = infiniteGrid problem

    -- The initial set of locations is the one place where the 'S' is
    initial = S.singleton . findS $ problem

    -- The total count from one tuple
    total (_, _, t, _) = t

    -- The new locations added this iteration
    newLocs (locs, _, _, _) = locs

    -- Every nth item in a list
    every n items = head items : every n (drop (n - 1) items)

-- | The number of A, C, and D tiles.
-- There's no need to compute the number of B or E tiles: there's always one of each.
--
-- This is the example in the diagram I drew:
-- >>> tileCounts 100 (V2 50 50) 660
-- (15,5,6)
--
-- This is the same example if the E block goes almost to the right edge
-- >>> tileCounts 100 (V2 50 50) 740
-- (20,6,7)
tileCounts :: Int -> Point -> Int -> (Int, Int, Int)
tileCounts tileSize startLoc n =
  (aCount, cCount, dCount)
  where
    aCount = aCountOnAxis + aCountNextRow * (aCountNextRow + 1) `div` 2
    cCount = aCountNextRow + 1
    dCount = aCountNextRow + 2

    -- Number of A tiles on the next row up from the axis
    aCountNextRow = (afterCorner `div` tileSize) - 1

    -- Number of iterations after reaching the corner of the first tile
    -- on the next row.
    afterCorner = afterEdge - (tileSize - sy)

    -- The number of A tiles on the axis may be one less than the full count,
    -- because the B tile will have been fully traversed, but may not have
    -- filled in all the way.
    aCountOnAxis = (afterEdge `div` tileSize) - 1

    -- Number of iterations after leaving the start tile on the X axis
    afterEdge = n - (tileSize - sx)

    -- Manhattan distance from start to edge of next tile
    startToEdge = tileSize - sx

    -- Starting x and y
    (V2 sx sy) = startLoc

-- | Information about one of the tile types.
data TileInfo = TileInfo
  { tileType :: Char, -- the name of this tile type
    tileCount :: Int, -- number of tiles of this type
    startLoc :: Point, -- the first location visited in the tile
    stepCount :: Int -- the number of steps taken into this tile
  }

instance Show TileInfo where
  show x = [tileType x] ++ "x" ++ show (tileCount x) ++ "(" ++ show (startLoc x) ++ "):" ++ show (stepCount x)

-- | TileInfo for all tiles in the upper-right quadrant, excluding the start tile
--
-- >>> tileInfos 5 (V2 3 3) 4
-- [Ex1(V2 1 3):2]
--
-- >>> tileInfos 5 (V2 3 3) 7
-- [Dx1(V2 1 1):2,Ex1(V2 1 3):5]
--
-- >>> tileInfos 5 (V2 3 3) 8
-- [Bx1(V2 1 3):6,Dx1(V2 1 1):3,Ex1(V2 1 3):1]
--
-- >>> tileInfos 5 (V2 3 3) 10
-- [Bx1(V2 1 3):8,Dx1(V2 1 1):5,Ex1(V2 1 3):3]
--
-- >>> tileInfos 5 (V2 3 3) 11
-- [Bx1(V2 1 3):9,Cx1(V2 1 1):6,Dx2(V2 1 1):1,Ex1(V2 1 3):4]
--
-- >>> tileInfos 5 (V2 3 3) 13
-- [Ax1(V2 1 1):10,Bx1(V2 1 3):6,Cx1(V2 1 1):8,Dx2(V2 1 1):3,Ex1(V2 1 3):1]
tileInfos :: Int -> Point -> Int -> [TileInfo]
tileInfos tileSize startLoc stepCount =
  catMaybes [aInfo, bInfo, cInfo, dInfo, eInfo]
  where
    -- there are A tiles if there are any steps between the start tile and the B or E tile
    aInfo =
      if 0 < aCountOnAxis
        then Just TileInfo {tileType = 'A', tileCount = aCount, startLoc = V2 1 1, stepCount = aSteps}
        else Nothing
      where
        aCount = aCountOnAxis + ((aCountNextRow * (aCountNextRow + 1)) `div` 2)
        aSteps = tileSize * 2 -- enough to visit entire tile

    -- there is a C tile if there are enough steps for a full tile before D
    cInfo =
      if tileSize < stepsIntoCorner
        then Just TileInfo {tileType = 'C', tileCount = cCount, startLoc = V2 1 1, stepCount = cSteps}
        else Nothing
      where
        cCount = (stepsIntoCorner - 1) `div` tileSize
        cSteps = (stepsIntoCorner `modOne` tileSize) + tileSize

    -- there is a D tile if there are any steps past the corner
    dInfo =
      if 0 < stepsAfterCorner
        then Just TileInfo {tileType = 'D', tileCount = dCount, startLoc = V2 1 1, stepCount = dSteps}
        else Nothing
      where
        dSteps = stepsIntoCorner `modOne` tileSize
        dCount = (stepsIntoCorner - 1) `div` tileSize + 1

    -- there is a B tile if there are enough steps for a full tile between A and E
    bInfo =
      if tileSize < stepsAfterEdge
        then Just TileInfo {tileType = 'B', tileCount = 1, startLoc = V2 1 sy, stepCount = bSteps}
        else Nothing
      where
        bSteps = (stepsAfterEdge `modOne` tileSize) + tileSize

    -- there is an E tile if there are enough steps to walk off the start tile
    eInfo =
      if 0 < stepsAfterEdge
        then Just TileInfo {tileType = 'E', tileCount = 1, startLoc = V2 1 sy, stepCount = eSteps}
        else Nothing
      where
        eSteps = stepsAfterEdge `modOne` tileSize

    -- Number of A tiles on the row jut above the X axis
    aCountNextRow = max 0 (((stepsIntoCorner - 1) `div` tileSize) - 1)

    -- Number of A tiles on the X axis
    aCountOnAxis = max 0 (((stepsAfterEdge - 1) `div` tileSize) - 1)

    -- Manhattan distance from start to the corner of the start tile
    stepsToCorner = (tileSize - sx) + (tileSize - sy)
    stepsAfterCorner = stepCount - stepsToCorner
    stepsIntoCorner = stepsAfterCorner - 1

    -- Manhattan distance from start to the edge of the start tile
    stepsToEdge = tileSize - sx
    stepsAfterEdge = stepCount - stepsToEdge

    -- Starting x and y
    (V2 sx sy) = startLoc

    modOne a b = ((a - 1) `mod` b) + 1

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
triangleify :: [(Int, a)] -> [(Int, a)]
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

    tileSize = assert isSquare (x1 - x0 + 1)
    isSquare = x1 - x0 == y1 - y0
    (V2 x0 y0, V2 x1 y1) = gridBounds g

-- | Run n steps in a tile, return (hasConverged, score)
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
runTile :: Grid -> Point -> Int -> (Bool, Int)
runTile g p n =
  (hasConverged, score)
  where
    -- We've converged on a steady state (module parity) if no new locations were added
    hasConverged = S.null justAdded

    -- The n-th step into the tile
    (justAdded, _, score, _) = steps !! n

    -- the sequence of steps, starting with 0 steps to make indexing easier
    steps = (S.empty, S.empty, 0, 0) : allPossible2 g (S.singleton p, S.empty, 1, 0)

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
