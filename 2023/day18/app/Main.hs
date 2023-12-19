module Main where

import Advent
  ( Grid (..),
    Point,
    gridBounds,
    gridFormat,
    gridGet,
    gridSet,
    neighbors,
    replace,
    run,
  )
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Debug.Trace
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

data Dir = R | L | D | U deriving (Read, Show)

-- | The input problem.
-- Each item in the list looks like  (R, 6, "70c710")
type Problem = [(Dir, Int, String)]

parse :: String -> Problem
parse =
  map parseLine . lines
  where
    parseLine s =
      (read a, read b, c)
      where
        [a, b, c] = words . replace '(' ' ' . replace ')' ' ' . replace '#' ' ' $ s

part1 :: Problem -> Int
part1 = length . filter (`elem` " #") . gridFormat . colorOutside . dig . compilePart1

part2 :: Problem -> Int
part2 = length

traceGrid :: Grid -> Grid
traceGrid g = trace (gridFormat g ++ "\n") g

-- | Translate the problem into a list of digging directions, one unit at a time.
--
-- >>> compilePart1 [(R, 2, "123456"), (D, 3, "abcdef")]
-- [R,R,D,D,D]
compilePart1 :: Problem -> [Dir]
compilePart1 =
  concatMap compileOne
  where
    compileOne (d, n, _) = replicate n d

-- | Start with one cube dug out, and dig following the instructions.
dig :: [Dir] -> Grid
dig =
  Grid . fst . foldl' digOne (M.insert (V2 1 1) '#' M.empty, V2 1 1)
  where
    digOne (m, p) d =
      (M.insert p' '#' m, p')
      where
        p' = p + delta d

-- | For one row of cubes, count how many are inside, including the edges
--
-- The state that gets folded into is: (prevChar, isInside, countSoFar)
--
-- >>> countRow "  # #  # #"
-- 6
--
-- >>> countRow "##  ###"
-- 7
countRow :: String -> Int
countRow =
  (\(_, _, n) -> n) . foldl' countOne (' ', False, 0)
  where
    countOne (c0, inside, n) c =
      (c, inside', n')
      where
        inside' = if c0 == ' ' && c == '#' then not inside else inside
        n' = if inside || c == '#' then n + 1 else n

-- | Color the "outside" (within a rectangle)
-- Start at one corner just beyond the bound of the grid, and let
-- the color flow to neighboring cells.
colorOutside :: Grid -> Grid
colorOutside grid =
  go [V2 (x0 - 1) (y0 - 1)] grid
  where
    -- the bounds of the existing grid
    (V2 x0 y0, V2 x1 y1) = gridBounds grid

    -- the bounds for the coloring are one bigger on each side
    inBounds (V2 x y) = x0 - 1 <= x && x <= x1 + 1 && y0 - 1 <= y && y <= y1 + 1

    -- visit a set of points in a grid, and return an updated grid
    go [] g = g
    go (v : vs) g =
      if needsColoring v g
        then go (neighbors v ++ vs) (gridSet v '*' g)
        else go vs g

    -- a cell needs coloring if it's in bounds and empty
    needsColoring p g = inBounds p && gridGet p g == ' '

delta :: Dir -> Point
delta R = V2 1 0
delta L = V2 (-1) 0
delta U = V2 0 (-1)
delta D = V2 0 1
