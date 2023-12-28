module Main where

import Advent
  ( Grid (..),
    Point,
    gridGet,
    gridMap,
    gridParse,
    only,
    run,
  )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse

part1 :: Problem -> Int
part1 problem =
  S.size . (!! 64) . iterate next $ initial
  where
    -- The initial set of locations is the one place where the 'S' is
    initial = S.singleton . findS $ problem

    -- Given a set of 'O's, where are the Os in the next iteration?
    next = S.fromList . filter isGarden . candidates

    -- Is the given location a garden plot?
    isGarden p = gridGet p problem `elem` ".S"

    -- Candidate locations where Os might be
    candidates = concatMap neighbors . S.toList

    -- Neighbors of a point
    neighbors p = map (p +) deltas

    -- Deltas to neighbors
    deltas = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

part2 :: Problem -> Int
part2 = M.size . gridMap

-- | Coordinates of the 'S' on the grid
findS :: Grid -> Point
findS = only . map fst . filter ((== 'S') . snd) . M.toList . gridMap
