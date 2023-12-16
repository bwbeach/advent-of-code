module Main where

import Advent
  ( Grid (..),
    Point,
    Rectangle (..),
    gridBounds,
    gridFormat,
    gridGet,
    gridMap,
    gridParse,
    rectangleContains,
    replace,
    run,
  )
import qualified Data.Map.Strict as M
import Debug.Trace
import Linear.V2 (V2 (..))
import Topograph (pairs)

main :: IO ()
main = run parse part1 part2

-- | A problem is a sparse grid, containing '#' and 'O', but no '.', and the bounds of the area rocks can roll in.
type Problem = (Grid, Rectangle)

parse :: String -> Problem
parse text =
  (grid, bounds)
  where
    grid = gridParse . replace '.' ' ' $ text
    bounds = Rectangle (V2 1 1) (V2 w h)
    w = length (head text')
    h = length text'
    text' = lines text

part1 :: Problem -> Int
part1 = score . rollNorth

part2 :: Problem -> Int
part2 =
  score . cycleAt 1000000000 . traceCycle . findCycle . iterate rollCycle
  where
    traceCycle c@(Cycle a b _) = trace (show (a, b)) c

-- | Keep applying the function until the result doesn't change
fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f =
  firstMatch . iterate f
  where
    firstMatch :: (Eq b) => [b] -> b
    firstMatch = fst . head . filter (uncurry (==)) . pairs

-- | Rolls all stones as far as they will go in the given direction.
--
-- Iterates through each row/column

-- | Roll round stones towards points at the beggining of a list of points
-- updateInOrder :: Grid -> [Point] -> Grid
-- updateInOrder grid =
--   foldl' go (Grid, [])
--   where
--     go (g, targets) p =
--       case (gridGet p g, targets) of
--         ('O', t : ts) -> ((exchange p t g), ts)
--         ('O', []) -> (g, [])
--         ('.', ts) -> (g, ts ++ [p])
--         ('#', _) -> (g, [])

-- | Move all round stones one square north, if possible.
rollNorth :: Problem -> Problem
rollNorth = roll (V2 0 (-1))

-- | Run one cycle
rollCycle :: Problem -> Problem
rollCycle = roll (V2 1 0) . roll (V2 0 1) . roll (V2 (-1) 0) . roll (V2 0 (-1))

-- | Roll stones as far as they will go in the given direction.
roll :: Point -> Problem -> Problem
roll delta = fixedPoint (rollOne delta)

-- | Move all round stones one square in the direction given, if possible.
rollOne :: Point -> Problem -> Problem
rollOne d (g, b) =
  (Grid . M.fromList . map roll . M.toList . gridMap $ g, b)
  where
    roll (p, c) =
      if c == 'O' && rectangleContains b p' && gridGet p' g == ' '
        then (p', c)
        else (p, c)
      where
        p' = p + d

-- | Score a grid
score :: Problem -> Int
score (g, _) =
  sum . map (scorePoint . fst) . filter ((== 'O') . snd) . M.toList . gridMap $ g
  where
    scorePoint (V2 _ y) = y1 - y + 1
    (_, V2 _ y1) = gridBounds g

-- | A cycle found in a sequence is (startOffset, count, map)
-- The map is a mapping from index (where index < startOffset + count) to the value at that index.
data Cycle a
  = Cycle Int Int (M.Map Int a)
  deriving (Show)

-- | Given an infinite list, finds the first repeating sequence
--
-- Assumes that a given value always has the same following value, which will
-- always be true for a list created with `iterate`.
--
-- >>> findCycle ([0, 1] ++ cycle [2, 3, 4, 5, 6])
-- Cycle 2 5 (fromList [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6)])
findCycle :: (Ord a) => [a] -> Cycle a
findCycle xs =
  go 0 M.empty xs
  where
    go i values (a : as) =
      case M.lookup a values of
        Just j -> Cycle j (i - j) (makeMap i)
        Nothing -> go (i + 1) (M.insert a i values) as

    makeMap n = M.fromList . zip [0 ..] . take n $ xs

-- | Finds the value at a given index in an infinite cycle
--
-- >>> cycleAt 100 $ findCycle ([0, 1] ++ cycle [2, 3, 4, 5, 6])
-- 5
cycleAt :: Int -> Cycle a -> a
cycleAt i (Cycle s n m) =
  m M.! index
  where
    index = s + ((i - s) `mod` n)
