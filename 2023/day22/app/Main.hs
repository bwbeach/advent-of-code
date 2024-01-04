module Main where

import Advent (run)
import Data.List (foldl', sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Tuple.Extra (both)
import Linear.V3 (V3 (..))

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

part1 :: Problem -> Point3
part1 problem =
  0 -- trace diagram 0
  where
    m = dropAllBlocks . map cubesInBlock $ problem
    diagram = reverse
    allPoints = concatMap bothPoints problem
    bothPoints (p1, p2) = [p1, p2]
    maxY = maximum . map (getY . fst) . M.toList $ m
    maxZ = maximum . map (getZ . fst) . M.toList $ m
    getX (V3 x _ _) = x
    getY (V3 _ y _) = y
    getZ (V3 _ _ z) = z

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

part2 :: Problem -> Int
part2 = length
