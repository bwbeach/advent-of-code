module Main where

import Advent (gridBounds, gridMap, gridParse, runTestAndInput)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Linear.V2 (V2 (..))

main :: IO ()
main = runTestAndInput gridParse part1 part2

part1 g =
  S.size $ S.intersection tobogganPath treeLocations
  where
    treeLocations = S.fromList . map fst . filter isTree . M.toList . gridMap $ g
    tobogganPath = S.fromList . map wrap . take height . iterate (+ V2 3 1) $ V2 1 1
    (_, V2 width height) = gridBounds g
    wrap (V2 x y) = V2 ((x - 1) `mod` width + 1) y
    isTree (_, c) = c == '#'

part2 g = 5
