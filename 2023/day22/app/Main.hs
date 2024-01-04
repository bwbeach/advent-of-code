module Main where

import Advent (gridFormat, gridFromList, mtsFromList, run)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (foldl', sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import qualified Data.Set as S
import Data.Tuple.Extra (both, first, second)
import Debug.Trace
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Topograph (pairs)

main :: IO ()
main = run parse part1 part2

-- A 3-D point
type Point3 = V3 Int

-- A block is defined by two points that specify the min/max for x, y, and z
type Block = (Point3, Point3)

-- A problem is a list of blocks in positions where they can fall.
type Problem = [Block]

-- | Parse the input file
parse :: String -> Problem
parse = map parseLine . lines

-- | Convert one line of input into a Block
--
-- >>> parseLine "0,1,6~2,1,6"
-- (V3 0 1 6,V3 2 1 6)
parseLine :: String -> Block
parseLine lineString =
  (parsePoint lhs, parsePoint rhs)
  where
    [lhs, rhs] = splitOn "~" lineString
    parsePoint pointString =
      V3 x y z
      where
        [x, y, z] = map read . splitOn "," $ pointString

part1 :: Problem -> Int
part1 problem =
  S.size canRemove
  where
    -- Build a map from block number to the set of blocks it's resting on
    isRestingOn = mtsFromList . makeIsRestingOn . dropAllBlocks . map cubesInBlock $ problem

    -- If block A is resting on a set of only one block B, then block B cannot be removed.
    cannotRemove = S.fromList . map (head . S.toList) . filter ((== 1) . S.size) . M.elems $ isRestingOn

    -- All block numbers
    allBlocks = S.fromList [1 .. length problem]

    -- Block numbers that can be removed
    canRemove = S.difference allBlocks cannotRemove

-- | Create a diagram with Y and Z axes, like in the problem description.
diagramXZ :: Problem -> String
diagramXZ problem =
  gridFormat grid
  where
    m = dropAllBlocks . map cubesInBlock $ problem
    yzToBlockNums = mtsFromList . map (first p3ToXz) . M.toList $ m
    grid = gridFromList . map (second (blocksToChar . S.toList)) . M.toList $ yzToBlockNums
    p3ToXz (V3 x _ z) = V2 x (negate z)

-- | Create a diagram with Y and Z axes, like in the problem description.
diagramYZ :: Problem -> String
diagramYZ problem =
  gridFormat grid
  where
    m = dropAllBlocks . map cubesInBlock $ problem
    yzToBlockNums = mtsFromList . map (first p3ToYz) . M.toList $ m
    grid = gridFromList . map (second (blocksToChar . S.toList)) . M.toList $ yzToBlockNums
    p3ToYz (V3 _ y z) = V2 y (negate z)

-- | When making diagrams, what letter to show, given the list of blocks in that square
blocksToChar :: [Int] -> Char
blocksToChar [] = '.'
blocksToChar [n] = ['A' ..] !! (n - 1)
blocksToChar _ = '?'

-- | "Cube" alias for clarity
type Cube = Point3

-- | All of the cubes in a block
--
-- >>> cubesInBlock (V3 1 1 4, V3 1 1 1)
-- [V3 1 1 1,V3 1 1 2,V3 1 1 3,V3 1 1 4]
cubesInBlock :: Block -> [Point3]
cubesInBlock (V3 x0 y0 z0, V3 x1 y1 z1) =
  [ V3 x y z
    | x <- values x0 x1,
      y <- values y0 y1,
      z <- values z0 z1
  ]
  where
    -- range that works for ends specified in either order
    values :: Int -> Int -> [Int]
    values a b = [(min a b) .. (max a b)]

-- | Infinite sequence of positions of a set of cubes as it falls.
--
-- >>> take 3 (falling [V3 1 1 3,V3 1 1 4])
-- [[V3 1 1 3,V3 1 1 4],[V3 1 1 2,V3 1 1 3],[V3 1 1 1,V3 1 1 2]]
falling :: [Cube] -> [[Cube]]
falling = iterate (map (mapz (\n -> n - 1)))

-- | Update the Z value in a point
mapz :: (a -> a) -> V3 a -> V3 a
mapz f (V3 x y z) = V3 x y (f z)

-- | Drops a set of cubes until it hits the ground or an already-falled cube.
-- The answer is the last position when falling that doesn't hit the ground or an existing cube.
--
-- >>> dropCubes M.empty [V3 3 4 5]
-- [V3 3 4 1]
--
-- >>> dropCubes (M.fromList [(V3 2 2 2, 1)]) [V3 2 2 5, V3 2 3 5]
-- [V3 2 2 3,V3 2 3 3]
dropCubes :: M.Map Point3 Int -> [Cube] -> [Cube]
dropCubes existing =
  last . takeWhile (all cubeOk) . falling
  where
    cubeOk p = M.notMember p existing && 0 < getZ p
    getZ (V3 _ _ z) = z

-- | Drop a set of cubes that has a block number and add it to the map.
--
-- >>> dropAndInsert M.empty ([V3 3 3 5, V3 3 3 6], 1)
-- fromList [(V3 3 3 1,1),(V3 3 3 2,1)]
dropAndInsert :: M.Map Point3 Int -> ([Cube], Int) -> M.Map Point3 Int
dropAndInsert existing (cubes, blockNum) =
  insertAll (dropCubes existing cubes) blockNum existing
  where
    insertAll ks v m0 = foldl' (\m k -> M.insert k v m) m0 ks

-- | Drop all of the blocks and create a map of them all.
--
-- >>> dropAllBlocks [[V3 2 3 4], [V3 2 3 7, V3 3 3 7], [V3 3 3 9]]
-- fromList [(V3 2 3 1,1),(V3 2 3 2,2),(V3 3 3 2,2),(V3 3 3 3,3)]
dropAllBlocks :: [[Cube]] -> M.Map Point3 Int
dropAllBlocks blocks =
  foldl' dropAndInsert M.empty (zip bottomToTop [1 ..])
  where
    bottomToTop = sortOn minZ blocks
    minZ = minimum . map getZ
    getZ (V3 _ _ z) = z

-- | Builds the relation is-resting-on as a list of pairs.
--
-- The map is sorted on points, which will group points with the same x and y,
-- and sort by z within that.  We can look at adjacent items in the list to
-- find one block sitting on another.
--
-- >>> makeIsRestingOn (M.fromList [(V3 3 3 6, 5), (V3 3 3 5, 5), (V3 3 3 3, 3), (V3 3 3 2, 2)])
-- [(3,2)]
makeIsRestingOn :: M.Map Point3 Int -> [(Int, Int)]
makeIsRestingOn =
  mapMaybe checkOne . pairs . M.toList
  where
    checkOne :: ((Point3, Int), (Point3, Int)) -> Maybe (Int, Int)
    checkOne ((V3 x1 y1 z1, b1), (V3 x2 y2 z2, b2)) =
      if x1 == x2 && y1 == y2 && z1 + 1 == z2 && b1 /= b2
        then Just (b2, b1)
        else Nothing

part2 :: Problem -> Int
part2 problem =
  trace diagram 0
  where
    diagram = unlines $ before ++ edgeSpecs ++ after
    before = ["digraph {"]
    after = ["}"]
    edges = makeIsRestingOn . dropAllBlocks . map cubesInBlock $ problem
    edgeSpecs = map edgeSpec edges
    edgeSpec (a, b) = "  " ++ blockName (a - 1) ++ " -> " ++ blockName (b - 1)
    blockName n =
      if n < 26
        then [letter n]
        else blockName (n `div` 26) ++ [letter (n `mod` 26)]
    letter n = ['A' ..] !! n
