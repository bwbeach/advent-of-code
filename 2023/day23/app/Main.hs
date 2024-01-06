module Main where

import Advent
  ( Grid,
    Point,
    gridBounds,
    gridFromList,
    gridGet,
    gridParse,
    gridToList,
    only,
    run,
  )
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Set as S
import Debug.Trace
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse

-- | Part 1
part1 :: Problem -> Int
part1 grid = longestPathInGraph (weightedGraph grid) startPos (fst (endState grid))

neighbors :: Point -> [Point]
neighbors p = [p + d | d <- directions]

-- | All four possible directions of movement
directions :: [Point]
directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

-- | Part 2
part2 :: Problem -> Int
part2 grid =
  longestPathInGraph (weightedGraph (replaceArrows grid)) startPos (fst (endState grid))

-- -- fromJust $ longestPath g' startPos S.empty (endState g')
-- M.size (traceShowId (weightedGraph sampleGrid))
-- where
--   g' = replaceArrows g

-- | Replace all arrows with dots in a grid
replaceArrows :: Grid -> Grid
replaceArrows =
  gridFromList . map updateOne . gridToList
  where
    updateOne (p, c) = (p, updateChar c)

    updateChar '<' = '.'
    updateChar '>' = '.'
    updateChar '^' = '.'
    updateChar 'v' = '.'
    updateChar c = c

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
sampleGrid = gridParse "#.###\n#.>.#\n#v#v#\n#.>.#\n###.#"

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
  S.Set Point -> -- The places we've been
  (Point, Direction) -> -- The current position
  Maybe Int -- The length of the path, if there is one
longestPath grid start history current@(curPos, _) =
  if curPos == start
    then Just 0
    else (+ 1) <$> maximumMaybes (map (longestPath grid start newHistory) prevs)
  where
    newHistory = S.insert curPos history
    prevs = filter (not . inHistory . fst) $ prevStates grid current
    inHistory p = p `S.member` history

-- | The maximum of a list of Maybes, or Nothing if there are no values.
--
-- >>> maximumMaybes [Nothing]
-- Nothing
--
-- >>> maximumMaybes [Just 3, Nothing, Just 6]
-- Just 6
maximumMaybes :: (Ord a) => [Maybe a] -> Maybe a
maximumMaybes =
  go . catMaybes
  where
    go [] = Nothing
    go ns = Just (maximum ns)

-- | Build a weighted graph from the input grid.
-- Vertices are at places in the maze where there's a choice about where to go.
-- Edges are weighted with the number of steps from one vertex to the next.
--
-- >>> weightedGraph sampleGrid
-- fromList [(V2 2 1,[(V2 2 2,1)]),(V2 2 2,[(V2 2 1,1),(V2 4 4,4)]),(V2 4 4,[(V2 4 5,1)]),(V2 4 5,[(V2 4 4,1)])]
--
-- >>> weightedGraph (replaceArrows sampleGrid)
-- fromList [(V2 2 1,[(V2 2 2,1)]),(V2 2 2,[(V2 2 1,1),(V2 4 4,4)]),(V2 4 4,[(V2 2 2,4),(V2 4 5,1)]),(V2 4 5,[(V2 4 4,1)])]
weightedGraph :: Grid -> M.Map Point [(Point, Int)]
weightedGraph grid =
  M.fromList $ map addOutgoing allVertices
  where
    allVertices = [startPoint, endPoint] ++ choicePoints grid
    vertexSet = S.fromList allVertices
    addOutgoing v = (v, findOutgoing grid vertexSet v)
    startPoint = V2 2 1
    endPoint = fst . endState $ grid

-- | Find all of the walkable locations in the grid that have more than two walkable neighbors
--
-- These are the places where you have a choice where to turn, and
-- they will be the vertices of the graph.
-- >>> choicePoints (replaceArrows sampleGrid)
-- [V2 2 2,V2 4 4]
choicePoints :: Grid -> [Point]
choicePoints grid =
  filter isChoicePoint . map fst . gridToList $ grid
  where
    isChoicePoint p = walkable (getAt p) && 2 < numWalkableNeighbors p
    numWalkableNeighbors = length . filter walkable . map getAt . neighbors
    getAt p = gridGet p grid
    walkable c = c `elem` ".<>^v"

-- | Find the outgoing edges from a vertex
findOutgoing :: Grid -> S.Set Point -> Point -> [(Point, Int)]
findOutgoing grid allVertices src =
  unique [edge | d <- directions, edge <- go src d 0]
  where
    -- All edges from a point `p` in a direction `d` having already taken `s` steps
    go p d s
      -- No answers if the direction of travel is not consistent with the place we're about to step.
      | not (charAndDirConsistent (gridGet p' grid) d) = []
      -- One answer if this step lands on one of the vertices
      | p' `S.member` allVertices = [(p', s + 1)]
      -- Otherwise, keep walking in any direction that is not backtracking
      | otherwise = [edge | d' <- directions, d' /= opposite d, edge <- go p' d' (s + 1)]
      where
        p' = p + d

-- | Unique values in a list
unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

-- | Find the longest path using a graph
--
-- >>> longestPathInGraph (weightedGraph sampleGrid) (V2 2 1) (V2 4 5)
-- 6
longestPathInGraph :: M.Map Point [(Point, Int)] -> Point -> Point -> Int
longestPathInGraph graph start end =
  fromJust $ go (S.singleton start) 0 start
  where
    go history steps current =
      if current == end
        then Just steps
        else maximumMaybes maybeNexts
      where
        maybeNexts =
          [ go (S.insert v history) (s + steps) v
            | (v, s) <- nexts current,
              not (v `S.member` history)
          ]
        nexts v = graph M.! v
