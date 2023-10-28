module Main where

import Advent (Grid, gridBounds, gridMap, gridParse, runTestAndInput)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Linear.V2 (V2 (..))

main :: IO ()
main = runTestAndInput gridParse part1 part2

part1 :: Grid -> Int
part1 grid = treesOnSlope grid (V2 3 1)

part2 :: Grid -> Int
part2 grid =
  product . map (treesOnSlope grid) $ [V2 1 1, V2 3 1, V2 5 1, V2 7 1, V2 1 2]

treesOnSlope :: Grid -> V2 Int -> Int
treesOnSlope grid slope =
  S.size $ S.intersection tobogganPath treeLocations
  where
    treeLocations = S.fromList . map fst . filter isTree . M.toList . gridMap $ grid
    tobogganPath = S.fromList . map wrap . take height . iterate (+ slope) $ V2 1 1
    (_, V2 width height) = gridBounds grid
    wrap (V2 x y) = V2 ((x - 1) `mod` width + 1) y
    isTree (_, c) = c == '#'
