module Main where

import Advent (Grid (..), gridBounds, gridFormat, gridGet, gridMap, gridParse, runTestAndInput)
import qualified Data.Map.Strict as M
import Debug.Trace
import Linear.V2 (V2 (..))
import Topograph (G (gDiff), pairs)

main :: IO ()
main = runTestAndInput parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse

part1 :: Problem -> Int
part1 =
  countOccupied . fst . head . filter (uncurry (==)) . pairs . iterate (lifeStep rule)
  where
    countOccupied = length . filter (== '#') . map snd . M.toList . gridMap

rule :: Char -> [Char] -> Char
rule c ns =
  case c of
    'L' -> if occupied == 0 then '#' else 'L'
    '#' -> if occupied >= 4 then 'L' else '#'
    _ -> c
  where
    occupied = count '#' ns

count :: (Eq a) => a -> [a] -> Int
count a = length . filter (== a)

part2 :: Problem -> Int
part2 _ = 5

-- | Computes the next step in a conway-life-style simulation.
lifeStep :: (Char -> [Char] -> Char) -> Grid -> Grid
lifeStep newCellState g =
  Grid result
  where
    result =
      M.fromList
        [ (V2 x y, c)
          | x <- [x0 - 1 .. x1 + 1],
            y <- [y0 - 1 .. y1 + 1],
            let p = V2 x y,
            let c = newCellState (get p) (map get (neighbors p)),
            c /= ' '
        ]
    get p = gridGet p g
    (V2 x0 y0, V2 x1 y1) = gridBounds g

neighbors :: V2 Int -> [V2 Int]
neighbors p = map (+ p) deltas
  where
    deltas =
      [ V2 x y
        | x <- [-1 .. 1],
          y <- [-1 .. 1],
          x /= 0 || y /= 0
      ]

-- [V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1, V2 0 (-1), V2 0 1, V2 1 (-1), V2 1 0, V2 1 1]