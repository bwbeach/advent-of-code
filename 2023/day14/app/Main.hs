module Main where

import Advent (Grid (..), gridBounds, gridGet, gridMap, gridParse, replace, run)
import qualified Data.Map.Strict as M
import Linear.V2 (V2 (..))
import Topograph (pairs)

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse . replace '.' ' '

part1 :: Problem -> Int
part1 = score . fixedPoint rollNorth

-- | Keep applying the function until the result doesn't change
fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f =
  firstMatch . iterate f
  where
    firstMatch :: (Eq b) => [b] -> b
    firstMatch = fst . head . filter (uncurry (==)) . pairs

-- | Move all round stones one square north, if possible.
rollNorth :: Grid -> Grid
rollNorth g =
  Grid . M.fromList . map roll . M.toList . gridMap $ g
  where
    roll (p@(V2 _ y), c) =
      if 1 < y && c == 'O' && gridGet p' g == ' '
        then (p', c)
        else (p, c)
      where
        p' = p + V2 0 (-1)

-- | Score a grid
score :: Grid -> Int
score g =
  sum . map (scorePoint . fst) . filter ((== 'O') . snd) . M.toList . gridMap $ g
  where
    scorePoint (V2 _ y) = y1 - y + 1
    (_, V2 _ y1) = gridBounds g

part2 :: Problem -> Int
part2 = length . M.toList . gridMap
