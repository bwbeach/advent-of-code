# 2023 Day 21: Step Counter

The problem today is to find all the possible locations an elf could
be in after taking N steps up/down/left right in a rectangular grid
while avoiding rocks (`#`).

## Part 1

### First Pass
The grid in part 1 is finite, and the numbers are small, so a straightforward solution works fine.  The set of possible locations at time T is represented
as a `Set Point`.  The function `next` takes the grid and a starting set and produces the set of possible locations at the next time.

```haskell
-- | Sequence of sets containing all possible positions at one time
allPossible :: (GenericGrid g) => g -> S.Set Point -> [S.Set Point]
allPossible problem =
  iterate next
  where
    -- Given a set of Os, where are the Os in the next iteration?
    next = S.fromList . filter isGarden . candidates

    -- Candidate locations where Os might be
    candidates = concatMap neighbors . S.toList

    -- Neighbors of a point
    neighbors p = map (p +) directions

    -- Deltas to neighbors
    directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

    -- Is the given location a garden plot?
    isGarden p = ggGet p problem `elem` ".S"
```

The answer to part 1 is then the size of the set after 64 iterations.

```haskell
part1 :: Problem -> Int
part1 problem =
  S.size . (!! 64) $ allPossible problem initial
  where
    -- The initial set of locations is the one place where the 'S' is
    initial = S.singleton . findS $ problem

-- | Coordinates of the 'S' on the grid
findS :: Grid -> Point
findS = only . map fst . filter ((== 'S') . snd) . M.toList . gridMap
```

The number of answers is proportional to the area of a shape of radius n, or O(n ^ 2).  This algorithm visits each of the locations each time through as the size o the set scales up.  Each visit is an O(log n) lookup in a `Set`.  That makes the whole thing O(n^3 log n).

### Second Pass

Can we do better?  I think so.

What if each step produces just the set of newly visited locations?  Say we have the last two sets: `mostRecent` is the set of locations just added, and `prior` is the set of locations before that.  The next set will be the neighbors of `mostRecent` that aren't in `prior`

Each time through, we'll process a set that is O(n) with O(log n) lookups, resulting in O(n^2 log n).  Not great, but not terrible for an answer of size O(n^2).

One thing to note is that there a two sets of visited locations: those that are an even number of steps away, and those that are an odd number of steps away.  Once the grid is fully visited, the set of nodes possible alternates between two different sets.

As the answers are being visited, this code keeps two counts: one for the current set, and one for the other set.

```haskell
-- | Sequence of states: (mostRecent, prior, mostRecentCount, priorCount)
-- `mostRecent` is the set of locations added in the last pass
-- `prior` is the set of locations added in the pass before that
-- `mostRecentCount` is the sum of sizes of `mostRecent` and all previous sets of the same parity.
-- `priorCount` is the sum of sizes of `prior` and all previous sets of the same parity.
allPossible2 :: (GenericGrid g) => g -> (S.Set Point, S.Set Point, Int, Int) -> [(S.Set Point, S.Set Point, Int, Int)]
allPossible2 problem =
  iterate next
  where
    -- What's the next tuple in the sequence?
    next (s1, s2, c1, c2) =
      (s0, s1, c2 + S.size s0, c1)
      where
        -- What locations get added in this round?
        s0 = S.fromList . filter isGarden . filter (not . (`S.member` s2)) . candidates $ s1

        -- Candidate locations next to the previous set
        candidates = concatMap neighbors . S.toList

        -- Neighbors of a point
        neighbors p = map (p +) directions

        -- Deltas to neighbors
        directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

        -- Is the given location a garden plot?
        isGarden p = ggGet p problem `elem` ".S"
```

## Part 2

Yikes!  In part 2, the grid becomes infinite by tiling the given grid, and the number of iterations becomes 26501365.  On an infinite grid, the size of the answer will grow by O(n^2).  So after ~26 million iterations, the answer will be on the order of 500 trillion.  Even an O(n) algorithm that touches them all will be far to slow.



