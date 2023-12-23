module Main where

import Advent
  ( Grid (..),
    Point (..),
    gridBounds,
    gridFormat,
    gridGet,
    gridSet,
    neighbors,
    replace,
    run,
  )
import Data.Hex (Hex (unhex))
import Data.List (elemIndex, foldl')
import Data.List.Extra (dropEnd)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Linear.V2 (V2 (..))
import RangeMap
  ( Range (..),
    RangeGrid,
    RangeMap,
    rangeSize,
    rgEmpty,
    rgFormat,
    rgInsert,
    rmRangeEnds,
    rmToList,
  )

main :: IO ()
main = run parse part1 part2

-- | The directions of movement
data Dir = R | L | D | U deriving (Read, Show)

-- | The input problem.
-- Each item in the list looks like  (R, 6, "70c710")
type Problem = [(Dir, Int, String)]

-- | One digging instruction to dig in a given direction
type DigInst = (Dir, Int)

-- | Parse the input file
parse :: String -> Problem
parse =
  map parseLine . lines
  where
    parseLine s =
      (read a, read b, c)
      where
        [a, b, c] = words . replace '(' ' ' . replace ')' ' ' . replace '#' ' ' $ s

-- | Run part 1
part1 :: Problem -> Int
part1 = solve . compilePart1

-- | Run part 2
part2 :: Problem -> Int
part2 = solve . compilePart2

-- | Translate the problem into a list of digging directions.
--
-- >>> compilePart1 [(R, 6, "70c710"), (D, 5, "0dc571")]
-- [(R,6),(D,5)]
compilePart1 :: Problem -> [(Dir, Int)]
compilePart1 =
  map compileOne
  where
    compileOne (d, n, _) = (d, n)

-- | Translate the problem into a list of digging directions.
--
-- >>> compilePart2 [(R, 6, "70c710"), (D, 5, "0dc571")]
-- [(R,461937),(D,56407)]
compilePart2 :: Problem -> [(Dir, Int)]
compilePart2 =
  map compileOne
  where
    compileOne (_, _, s) = (getDir (last s), fromHex (dropEnd 1 s))

    getDir '0' = R
    getDir '1' = D
    getDir '2' = L
    getDir '3' = U

    fromHex =
      foldl' addDigit 0
      where
        addDigit n c = n * 16 + fromJust (elemIndex c "0123456789abcdef")

-- | Given digging instructions, what's the size of the lagoon?
solve :: [(Dir, Int)] -> Int
solve = score . dig

-- | Count the number of cells in the lagoon.
-- The RangeGrid divides the area up into vertical columns, each of which has a width.
score :: RangeGrid Int Char -> Int
score =
  sum . map scoreCol . rmToList
  where
    scoreCol (r, colMap) = rangeSize r * scoreRanges (rmToList colMap)

-- | Score one sequence of ranges in a cross-section of the lagoon.
--
-- The state that gets folded into is:
--   ( scoreSoFar, -- number of wall and interior cells so far
--     corner,     -- unresolved corner character 'L' or 'R', or '#' if in a wall, or ' ' if not
--     inside      -- are we currently inside the wall?
--   )
--
-- >>> scoreRanges [(Range 1 1, '#'), (Range 2 3, ' '), (Range 4 4, '#')]
-- 4
--
-- >>> scoreRanges [(Range 1 1, 'L'), (Range 2 2, 'R'), (Range 3 4, ' ')]
-- 4
--
-- >>> scoreRanges [(Range 1 1, 'L'), (Range 2 2, 'L'), (Range 3 4, ' ')]
-- 2
--
-- >>> scoreRanges [(Range 1 1, '#'), (Range 5 5, '#')]
-- 5
scoreRanges :: [(Range Int, Char)] -> Int
scoreRanges =
  sum . getScore . foldl' scoreRange ([], ' ', False) . addGaps
  where
    getScore (s, _, _) = s
    scoreRange (s0, corner0, inside0) (r, c) =
      (s, corner, inside)
      where
        -- updated score
        s = (if counts then rangeSize r else 0) : s0
        -- does this range add to the score?
        counts = c /= ' ' || inside0
        -- new corner state, given the previous corner state and the next char
        (corner, inside) = case (corner0, c) of
          (' ', ' ') -> (' ', inside0)
          (' ', 'L') -> ('L', inside0)
          (' ', 'R') -> ('R', inside0)
          (' ', '#') -> (' ', not inside0)
          ('L', 'L') -> (' ', inside0)
          ('L', 'R') -> (' ', not inside0)
          ('L', '#') -> ('L', inside0)
          ('R', 'L') -> (' ', not inside0)
          ('R', 'R') -> (' ', inside0)
          ('R', '#') -> ('R', inside0)
          (a, b) -> error ("bad update to corner: " ++ show (corner0, c))

-- | Add spaces to gaps between ranges, so the scoring function can score them.
--
-- >>> addGaps [(Range 1 3, 'A'), (Range 7 9, 'B')]
-- [(Range 1 3,'A'),(Range 4 6,' '),(Range 7 9,'B')]
addGaps [] = []
addGaps [kv] = [kv]
addGaps (kv0@(Range a b, x) : kv1@(Range c d, y) : kvs) =
  if b + 1 == c
    then kv0 : addGaps (kv1 : kvs)
    else kv0 : (Range (b + 1) (c - 1), ' ') : addGaps (kv1 : kvs)

-- | Dig the perimiter following instructions
--
-- The result is a grid with '#' along the edges, and 'R' and 'L' at the turn locations.
-- Example:
--
--    R######R
--    #      #
--    #    L#R
--    R####R
--
-- The state carried through the process is:
--   ( prevDir,  -- the direction of digging in the previous step
--     location, -- the point naming the current location
--     grid      -- the grid so far
--   )
--
-- Adding a border segment is done in two steps:
--   - draw the full length of the segment with '#'
--   - replace the first cell of the wall with 'L' or 'R'
--
-- This means that after drawing all segments, the starting point will
-- have been overwritten with '#', so it needs to get reset to the direction
-- of the turn that would happen going from the last segment to the first.
--
-- >>> rgFormat (dig [(R, 7), (D, 2), (L, 2), (D, 1), (L, 5), (U, 3)]) id
-- "R####R\n#    #\n#  L#R\nR##R  \n"
dig :: [(Dir, Int)] -> RangeGrid Int Char
dig instrs =
  setLastTurn . getGrid . foldl' digOne initial $ instrs
  where
    -- initial state, assuming a closed loop so the direction at start is the last direction
    initial = (lastDir, V2 1 1, rgEmpty)
    -- set the direction of the last/first turn at the starting point
    setLastTurn = rgInsert (Range 1 1) (Range 1 1) (turnChar lastDir firstDir)
    firstDir = fst . head $ instrs
    lastDir = fst . last $ instrs
    getGrid (_, _, g) = g
    digOne (d0, p0@(V2 x0 y0), g0) (d, n) =
      (d, p1, g)
      where
        g =
          rgInsert (range x0 x0) (range y0 y0) (turnChar d0 d)
            . rgInsert (range x0 x1) (range y0 y1) '#'
            $ g0
        p1@(V2 x1 y1) = p0 + fmap (* n) (delta d)
        range a b = Range (min a b) (max a b)

-- | Given a sequence of two directions, what kind of turn happened?
turnChar :: Dir -> Dir -> Char
turnChar U R = 'R'
turnChar U L = 'L'
turnChar R D = 'R'
turnChar R U = 'L'
turnChar D L = 'R'
turnChar D R = 'L'
turnChar L U = 'R'
turnChar L D = 'L'

-- | Turn a direction into a unit vector in that direction.
delta :: Dir -> Point
delta R = V2 1 0
delta L = V2 (-1) 0
delta U = V2 0 (-1)
delta D = V2 0 1

-- | Show the contents of the grid
traceGrid :: RangeGrid Int Char -> RangeGrid Int Char
traceGrid g = trace (rgFormat g id ++ "\n") g
