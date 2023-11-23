module Main where

import Advent
  ( LifeGrid (lgCandidates, lgCells, lgGet, lgNew),
    Point,
    gridMap,
    gridParse,
    run,
  )
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (first)
import Debug.Trace
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

main :: IO ()
main = run parse part1 part2

type Point3 = V3 Int

point3From2 :: Point -> Point3
point3From2 (V2 x y) = V3 x y 0

point3X :: V3 a -> a
point3X (V3 x _ _) = x

point3Y :: V3 a -> a
point3Y (V3 _ y _) = y

point3Z :: V3 a -> a
point3Z (V3 _ _ z) = z

type Grid3 = M.Map Point3 Char

grid3Bounds :: Grid3 -> (Point3, Point3)
grid3Bounds g =
  (V3 (minimum xs) (minimum ys) (minimum zs), V3 (maximum xs) (maximum ys) (maximum zs))
  where
    ps = M.keys g
    xs = map point3X ps
    ys = map point3Y ps
    zs = map point3Z ps

grid3Format :: Grid3 -> String
grid3Format g =
  intercalate "\n" . map formatSlice $ [z0 .. z1]
  where
    (V3 x0 y0 z0, V3 x1 y1 z1) = grid3Bounds g
    formatSlice z = unlines . map (formatRow z) $ [y0 .. y1]
    formatRow z y = map (formatCell z y) [x0 .. x1]
    formatCell z y x = fromMaybe '.' (M.lookup (V3 x y z) g)

type Problem = Grid3

parse :: String -> Problem
parse =
  M.fromList . map (first point3From2) . M.toList . gridMap . gridParse . replace '.' ' '
  where
    replace a b = map (\x -> if x == a then b else x)

part1 :: Problem -> Int
part1 = length . M.toList

instance LifeGrid Grid3 where
  lgNew _ = M.fromList
  lgGet = M.lookup
  lgCells = M.keys
  lgCandidates = grid3Candidates

part2 :: Problem -> Int
part2 = length
