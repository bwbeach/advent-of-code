module Main where

import Advent
  ( Grid,
    Point,
    gridBounds,
    gridGet,
    gridParse,
    gridToList,
    run,
  )
import Debug.Trace
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse

-- | Part 1
part1 :: Problem -> Int
part1 grid = longestPath grid startPos (endState grid)

neighbors :: Point -> [Point]
neighbors p = [p + d | d <- directions]

-- | All four possible directions of movement
directions :: [Point]
directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

-- | Part 2
part2 :: Problem -> Int
part2 _ = 0

-- | Type alias for clarity, to distinguish direction from location.
type Direction = V2 Int

-- | Where the hike will be at the end.
-- It will be in the goal location, having just stepped in the direct of out of the maze (down; positive y).
endState :: Grid -> (Point, Direction)
endState grid =
  (V2 (x1 - 1) y1, V2 0 1)
  where
    (_, V2 x1 y1) = gridBounds grid

-- | Position where the hike starts.
startPos :: Point
startPos = V2 2 1

-- | A grid to use in examples
--
--    #.###
--    #.>.#
--    #v#v#
--    #.>.#
--    ###.#
sampleGrid :: Grid
sampleGrid = gridParse "#.###\n#.>.#\n#v#v#\n#.>.#\n### #"

-- | All possible previous states.
-- The direction in the state is the direction of travel *leaving* the current location,
-- going to the next one.  This function is looking for the previous position, and that
-- direction is not yet chosen, but must be consistent with the arrow on the current
-- position.
--
-- Regular variables are the current state.  Prime vars are the previous state (p', d').
--
-- >>> prevStates sampleGrid (V2 3 2, V2 1 0)
-- [(V2 2 2,V2 1 0)]
--
-- >>> prevStates sampleGrid (V2 2 2,V2 1 0)
-- [(V2 2 1,V2 0 1)]
prevStates :: Grid -> (Point, Direction) -> [(Point, Direction)]
prevStates grid (p, d) =
  [ (p', d')
    | d' <- directions,
      d' /= opposite d, -- no backsies
      let p' = p + opposite d',
      isDirAllowed p d',
      isDirAllowed p' d'
  ]
  where
    -- Is the direction of travel consistent with the arrows?
    isDirAllowed px = charAndDirConsistent (gridGet px grid)

-- | The opposite of a direction
--
-- >>> opposite (V2 1 0)
-- V2 (-1) 0
opposite :: Direction -> Direction
opposite = fmap negate

charAndDirConsistent :: Char -> Direction -> Bool
-- charAndDirConsistent ' ' _ = True -- to allow getting TO the start location
charAndDirConsistent '.' _ = True
charAndDirConsistent '<' (V2 (-1) 0) = True
charAndDirConsistent '>' (V2 1 0) = True
charAndDirConsistent '^' (V2 0 (-1)) = True
charAndDirConsistent 'v' (V2 0 1) = True
charAndDirConsistent c d = False

-- | Search for the longest path to a given state.
longestPath ::
  Grid -> -- The maze we're exploring
  Point -> -- The starting position
  (Point, Direction) -> -- The current position
  Int -- The length of the path
longestPath grid start current@(curPos, _) =
  if curPos == start
    then 0
    else 1 + maximum (map (longestPath grid start) (prevStates grid current))
