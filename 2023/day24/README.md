# 2023 Day 24

The problem has a bunch of hailstones with starting positions and velocities.

The test input and the real input have different areas to look for collisions in.  I'm going to prepend those ranges as a first line in the files.

## Parsing 

First, some type aliases to keep track of what's what:

```haskell
type TargetRange = (Integer, Integer)

type Point = V3 Integer

type Velocity = V3 Integer

type Hailstone = (Point, Velocity)

type Problem = (TargetRange, [Hailstone])
```

Parsing is straightforward:

```haskell
-- Parse the input text
--
-- >>> parse "11 22\n19, 13, 30 @ -2,  1, -2\n"
-- ((11,22),[(V3 19 13 30,V3 (-2) 1 (-2))])
parse :: String -> Problem
parse text =
  (parseTarget (head items), map parseHail (tail items))
  where
    items = lines text
    parseTarget s =
      (a, b)
      where
        [a, b] = map read . words $ s
    parseHail s =
      (a, b)
      where
        [a, b] = map parseV3 . splitOn "@" . filter (/= ',') $ s
    parseV3 s =
      V3 x y z
      where
        [x, y, z] = map read . words $ s
```

## Part 1

I have no idea where part 2 is going to go, so let's just focus on the specifics of part 1.  The goal is to project the path of each hailstone onto the x/y plane, and find the locations where those lines intersect, and only conside the intersections that happen in the future.

### Projecting onto x/y plane

```haskell
-- | Projects a 3D thing onto the x/y plane.
dropZ :: V3 a -> V2 a
dropZ (V3 x y _) = V2 x y

type Point2 = V2 Integer

type Velocity2 = V2 Integer

type Hailstone2 = (Point2, Velocity2)

-- | Projects a Hailstone onto the x/y plane
hailDropZ :: Hailstone3 -> Hailstone2
hailDropZ = both dropZ
```

Actually, after doing the next two sections, Rational numbers will be easier to deal with, because that's how the rest of the calculations will be done.  So let's convert now.

```haskell
type Point2 = V2 Rational

type Velocity2 = V2 Rational

type Hailstone2 = (Point2, Velocity2)

-- | Projects a Hailstone onto the x/y plane
hailDropZ :: Hailstone3 -> Hailstone2
hailDropZ = both (dropZ . fmap fromIntegral)
```

### Convert lines to y = mx + b

The slope of the line is determined by the velocity portion.  Then a little algebra will solve `y = mx + b` for y: `b = y - mx`.

```haskell
-- | A line defined by slope (m) and intercept (b): y = mx + b
data SILine = SILine
  { m :: Rational,
    b :: Rational
  }
  deriving (Show)

-- | Get the line for a hailstone
--
-- >>> hailLine (V2 3 4, V2 1 2)
-- SILine {m = 2 % 1, b = (-2) % 1}
hailLine :: Hailstone2 -> SILine
hailLine (V2 x y, V2 vx vy) =
  SILine {m = slope, b = intercept}
  where
    slope = vy / vx
    intercept = y - slope * x
```

### Finding the intersection of two lines 

OK.  I probably should have thought this through before, but now it's time to figure out where two lines intersect. 

Two lines intersect when their y values are the same: 

  - `mx + b = m'x + b'`
  - `mx - m'x = b' - b`
  - `(m - m') x = b' - b`
  - `x = (b' - b) / (m - m')`

Once we have an X, we can use either line's formula to calculate Y.

```haskell
-- | Find the intesection point for two lines
--
-- >>> intersect (SILine {m=1, b=1}) (SILine {m=2, b=0})
-- V2 (1 % 1) (2 % 1)
intersect :: SILine -> SILine -> V2 Rational
intersect line line' =
  V2 x y
  where
    SILine {m = m, b = b} = line
    SILine {m = m', b = b'} = line'
    x = (b' - b) / (m - m')
    y = m * x + b
```

### In the future?

We'll need a test to see if the intersection point is in the future for a given hailstone.  That will happen if the intersection point is in the same direction as the velocity vector.  We know the velocity vector and the vector from the start point to the intersection point are colinear, so we can just check that the x difference is in the same direction.  "Same direction" here can mean: having the same sign. 

```haskell
-- | Is the given point on a hailstone's line in its future?
inFuture :: Hailstone2 -> Point2 -> Bool 
inFuture (V2 hx _, V2 hvx _) (V2 x _) =
    signum hvx == signum (x - hx)
```

### Check 'em 

Now we can check all pairs of hailstones for collisions in the desired range. 

We'll need all pairs of hailstones.  Seems like there should be a library function for this, but searching for `[a] -> [(a, a)]` on Hoogle didn't find it.

```haskell
-- | All pairs of two elements of a list, with no repeats.
-- First element of pair always appears in the list before the second element.
--
-- >>> allPairs [1, 2, 3, 4]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
allPairs :: [a] -> [(a, a)]
allPairs =
  concatMap headWithRest . tails
  where
    headWithRest [] = []
    headWithRest [_] = []
    headWithRest (a : as) = map (a,) as
```

And now we have the tools to write the solution to the problem:

```haskell
-- | Day 24, Part 1
part1 :: Problem -> Int
part1 (range, hailstones) =
  length . filter (intersectInFutureInRange range) . allPairs . map hailDropZ $ hailstones

-- | Do two hailstones intersect in the future?
intersectInFutureInRange :: (Integer, Integer) -> (Hailstone2, Hailstone2) -> Bool
intersectInFutureInRange range (a, b) =
  inFuture a p && inFuture b p && inRange p
  where
    p = hailLine a `intersect` hailLine b
    inRange (V2 x y) = low <= x && x <= high && low <= y && y <= high
    (low, high) = both toRational range
```

And... it fails with `Ratio has zero denominator`.  After adding some tracing, I found that the test data has parallel lines that don't intersect.

Let's update `intersect` to handle that:

```haskell
-- | Find the intesection point for two lines
--
-- >>> intersect (SILine {m=1, b=1}) (SILine {m=2, b=0})
-- Just (V2 (1 % 1) (2 % 1))
--
-- >>> intersect (SILine {m=1, b=1}) (SILine {m=1, b=0})
-- Nothing
intersect :: SILine -> SILine -> Maybe (V2 Rational)
intersect line line' =
  if m == m'
    then Nothing -- parallel or the same line
    else Just (V2 x y)
  where
    SILine {m = m, b = b} = line
    SILine {m = m', b = b'} = line'
    x = (b' - b) / (m - m')
    y = m * x + b
```

And update `intersectInFutureInRange`:

```haskell
-- | Do two hailstones intersect in the future?
intersectInFutureInRange :: (Integer, Integer) -> (Hailstone2, Hailstone2) -> Bool
intersectInFutureInRange range (a, b) =
  case intersection of
    Nothing -> False
    Just p -> inFuture a p && inFuture b p && inRange p
  where
    intersection = hailLine a `intersect` hailLine b
    inRange (V2 x y) = low <= x && x <= high && low <= y && y <= high
    (low, high) = both toRational range
```

Now it works.

