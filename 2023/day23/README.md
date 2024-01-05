# 2023 Day 23: A Long Walk

## Part 1 

The problem is to find the longest path through a maze, with some one-way sections, and without revisiting any locations.  This is the same as the [Longest Path Problem](https://en.wikipedia.org/wiki/Longest_path_problem) described in Wikipedia.  It says that it can be solved for a _directed acyclic_ graph in O(n) time.  The problem is NP-hard for undirected graphs.

So -- is the graph directed and acyclic?  The test data looks that way after a short inspection.  It looks like every location with more than two neighbors, where there's a choice about where to go, is surrounded be arrows.  Let's write some code to find out. 

### Parsing 

My advent library includes parsing of character grids.

```haskell
type Problem = Grid

parse :: String -> Problem
parse = gridParse
```

### Is it a DAG? 

This ugly code checks whether the grid is consistent with a DAG, which is the case if every cell meets any of these criteria:

 - it's a wall (`#`) 
 - 2 or fewer neighbors is a wall ("or fewer" for entry and exit locations)
 - every neighbor is an arrow

```haskell
part1 :: Problem -> Int
part1 problem =
  length . traceShowId . filter (not . isConsistentWithDag . fst) . gridToList $ problem
  where
    boolToInt a = if a then 1 else 0

    isConsistentWithDag p =
      getAt p == '#' || length nonWall < 3 || all isArrow nonWall
      where
        nc = neighborChars p
        nonWall = filter (/= '#') nc
        isArrow c = c `elem` "<^>v"

    neighborChars = filter (/= ' ') . map getAt . neighbors

    getAt p = gridGet p problem

neighbors :: Point -> [Point]
neighbors p = [p + d | d <- directions]

directions :: [Point]
directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]
```

Running this says that both the test case and the real input are DAGs.

## Convert to DAG

Maybe not?

Looking around, I haven't found any Haskell packages that handle DAGs with weighted edges, so converting it to a full-on graph may not be worth the trouble.  Maybe just implement the algorithm directly using recursion and memoization...

## Directly implement

We're going to want to search backwards, and when there's a choice pick the one with the longer path.  At each location, it'll need to look at all the preceding locations, make a recursive call on each one to get its length, and pick the highest one.

### Choices for previous location 

The first step will be to implement "what can the previous locations be?"  The code should not reverse course, so maybe it should carry along the current direction as it goes.

Both the test and the input start at the lower right, so we'll just hard-code that.  The direction in the state is the direction we stepped to get to this location; the previous point is known. 

```haskell
-- | Where the hike will be at the end. 
-- It will be in the goal location, headed out of the maze (down; positive y).
endState :: Grid -> (Point, Direction)
endState grid = 
    (V2 (x1 - 1) y1, V2 0 1)
    where 
        (_, V2 x1 y1) = gridBounds grid
```

And we'll probably also need the starting position to know when the seach is done.

```haskell
-- | Position where the hike starts. 
startPos :: Point 
startPos = V2 2 1
```

Working backwards from the end, we'll need to find all of the previous positions that don't just reverse direction, and are consistent with the arrows.  This code checks for arrows at both positions, which is probably not necessary.

```haskell
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
```

### Longest path

Now, simple recursion should find us the longest path.  My guess is that it will be fast enough without memoization for the test case, but not for the real input.

```haskell
-- | Part 1
part1 :: Problem -> Int
part1 grid = longestPath grid startPos (endState grid)

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
```

It turns out that's plenty fast.