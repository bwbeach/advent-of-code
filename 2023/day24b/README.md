# Day 24, Part 2, Second try

In my first try, I didn't see how to reduce the problem to a system of linear equations.  After finishing my optimization implementation, I read the [Reddit thread](https://www.reddit.com/r/adventofcode/comments/18pnycy/2023_day_24_solutions).  A bunch of people figured out how to isolate the non-linear part of the equations and get rid of it.  Now I want to try it myself, and also try writing math in markdown.

## Naming things

The thing we're looking for is the starting position and velocity of the rock such that it intersects all of the hailstones on their paths at the same time they are there.  We'll use $\vec{p} = [x, y, z]$ as the position of the rock, and $\vec{v} = [a, b, c]$ as the velocity of the rock.  These are the variables we want to solve for.

We'll use capital letters for the position, $\vec{P_1} = [X_1, Y_1, Z_1]$, and velocity, $\vec{V_1} = [A_1, B_1, C_1]$ of the hailstones.  The subscript is the number of the hailstone.  These are constants provided as the problem input.

## What we're looking for

For all of the hailstones, there needs to be a time t such that this is true:

$\vec{p} + t\vec{v} = \vec{P_i} + t\vec{V_i}$

This can be broken into three separate equations:

$p_x + tv_x = P_{ix} + tV_{ix}$

$p_y + tv_y = P_{iy} + tV_{iy}$

$p_z + tv_z = P_{iz} + tV_{iz}$

## The first trick

The first trick is to get rid of $t$ by solving those equations for t:

$p_x + tv_x = P_{ix} + tV_{ix}$

$tv_x - tV_{ix} = P_{ix} - p_x$

$t = \frac{P_{ix} - p_x}{v_x - V_{ix}}$

Because all of the equations use the same t:

$t = \frac{P_{ix} - p_x}{v_x - V_{ix}} = \frac{P_{iy} - p_y}{v_y - V_{iy}} = \frac{P_{iz} - p_z}{v_z - V_{iz}}$

Each of these fractions is saying that the time needed for the rock and hailstone to be in the same place is the distance between them divided by their relative velocities.  And the result must be the same along each axis.

## Tedious stuff

Now, let's take a pair of those fractions and multiply them out.

$\frac{P_{ix} - p_x}{v_x - V_{ix}} = \frac{P_{iy} - p_y}{v_y - V_{iy}}$

$(P_{ix} - p_x)(v_y - V_{iy}) = (P_{iy} - p_y)(v_x - V_{ix})$

$P_{ix}v_y - P_{ix}V{iy} - p_xv_y + p_xV{iy} = P_{iy}v_x - P_{iy}V_{ix} - p_yv_x + p_yV_{ix}$

## The second trick

We can pull the non-linear parts to one side:

$p_yv_x - p_xv_y = - V_{iy}p_x + V_{ix}p_y + P_{iy}v_x - P_{ix}v_y + (P_{ix}V_{iy} - P_{iy}V_{ix})$

The second trick is to notice, which I would not have without help, that the left hand side is just about the rock, and has the same value no matter what hailstone you pick.  So the right hand side for two different hailstones must be the same.  Let's use the subscript $k$ for the other hailstone:

$- V_{iy}p_x + V_{ix}p_y + P_{iy}v_x - P_{ix}v_y + (P_{ix}V_{iy} - P_{iy}V_{ix}) = - V_{ky}p_x + V_{kx}p_y + P_{ky}v_x - P_{kx}v_y + (P_{kx}V_{ky} - P_{ky}V_{kx})$

Now we can collect like terms and have a linear equation in four variables:

$(V_{ky} - V_{iy})p_x + (V_{ix} - V_{kx})p_y + (P_{iy} - P_{ky})v_x + (P_{kx} - P_{ix})v_y = P_{kx}V_{ky} - P_{ky}V_{kx} - P_{ix}V_{iy} + P_{iy}V_{ix}$

There are lots of hailstones.  Plenty to fill out four equations in four unknowns.

## Code: parsing the input

A bunch of type definitions for clarity, and parsing the input:

```haskell
type Point3 = V3 Rational

type Velocity3 = V3 Rational

type Hailstone3 = (Point3, Velocity3)

type Problem = [Hailstone3]

-- Parse the input text
--
-- >>> parse "19, 13, 30 @ -2,  1, -2\n"
-- [(V3 (19 % 1) (13 % 1) (30 % 1),V3 ((-2) % 1) (1 % 1) ((-2) % 1))]
parse :: String -> Problem
parse =
  map parseHail . lines
  where
    parseHail s =
      (a, b)
      where
        [a, b] = map parseV3 . splitOn "@" . filter (/= ',') $ s
    parseV3 s =
      V3 x y z
      where
        [x, y, z] = map (fromIntegral . read) . words $ s
```

## Code: flattening

The equations we make will work in 2D, so we'll want to flatten 3D positions and vectors onto a plane.

```haskell
type Point2 = V2 Rational

type Velocity2 = V2 Rational

type Hailstone2 = (Point2, Velocity2)

-- | Convert a hailstone to 2D by retaining just x and y
dropZ :: Hailstone3 -> Hailstone2
dropZ (V3 x y _, V3 dx dy _) = (V2 x y, V2 dx dy)

-- | Convert a hailstone to 2D by retaining just x and z
dropY :: Hailstone3 -> Hailstone2
dropY (V3 x _ z, V3 dx _ dz) = (V2 x z, V2 dx dz)

-- | Convert a hailstone to 2D by retaining just y and z
dropX :: Hailstone3 -> Hailstone2
dropX (V3 _ y z, V3 _ dy dz) = (V2 y z, V2 dy dz)
```

## Code: making the equation

This code calculation the coefficients from the equation above.

```haskell
-- | Make one equation, based on two hailstones
-- See README.md for details.
--
-- >>> makeEquation (dropZ hail1) (dropZ hail2)
-- [(-5) % 1,3 % 1,(-15) % 1,9 % 1,(-5) % 1]
--
-- >>> makeEquation (dropX hail1) (dropX hail2)
-- [(-4) % 1,5 % 1,(-16) % 1,15 % 1,48 % 1]
makeEquation :: Hailstone2 -> Hailstone2 -> [Rational]
makeEquation (V2 pix piy, V2 vix viy) (V2 pkx pky, V2 vkx vky) =
  [vky - viy, vix - vkx, piy - pky, pkx - pix, pkx * vky - pky * vkx - pix * viy + piy * vix]
```

## Code: testing it

I never trust that I haven't mistyped something or flipped a sign until I check it.  This code creates a rock position and velocity, creates hailstones by working backwards from that, and then checks that the equation is consistent with the rock and hail.

(I had, in fact, typed an `x` where there should have been a `y` in one spot.  These tests caught it and I fixed it.)

```haskell
-- | Rock to use in examples.
aRock :: Hailstone3
aRock = (V3 7 11 13, V3 2 3 5)

-- | Make a hailstone to use in examples with the given velocity that intersects the rock at the given time.
aHail :: Velocity3 -> Integer -> Hailstone3
aHail v t =
  (intersection - scale t v, v)
  where
    (rp, rv) = aRock
    intersection = rp + scale t rv
    scale n = fmap (* fromIntegral n)

-- | The first hailstone to use in examples.
--
-- >>> hail1
-- (V3 (7 % 1) (11 % 1) (21 % 1),V3 (2 % 1) (3 % 1) (1 % 1))
hail1 :: Hailstone3
hail1 = aHail (V3 2 3 1) 2

-- | The second hailstone to use in examples
--
-- >>> hail2
-- (V3 (16 % 1) (26 % 1) (37 % 1),V3 ((-1) % 1) ((-2) % 1) ((-3) % 1))
hail2 :: Hailstone3
hail2 = aHail (V3 (-1) (-2) (-3)) 3

-- | Does the equation for the two test hailstones work in the XY plane?
--
-- >>> tryEquationXY hail1 hail2
-- ((-5) % 1,(-5) % 1)
tryEquationXY :: Hailstone3 -> Hailstone3 -> (Rational, Rational)
tryEquationXY h1 h2 =
  (a * x + b * y + c * dx + d * dy, e)
  where
    [a, b, c, d, e] = makeEquation (dropZ h1) (dropZ h2)
    (V2 x y, V2 dx dy) = dropZ aRock

-- | Does the equation for the two test hailstones work in the XZ plane?
--
-- >>> tryEquationXZ hail1 hail2
-- (24 % 1,24 % 1)
tryEquationXZ :: Hailstone3 -> Hailstone3 -> (Rational, Rational)
tryEquationXZ h1 h2 =
  (a * x + b * z + c * dx + d * dz, e)
  where
    [a, b, c, d, e] = makeEquation (dropY h1) (dropY h2)
    (V2 x z, V2 dx dz) = dropY aRock
```

## Solving the equations

I get a bit lost finding libraries in Haskell.  The documentation is pretty sparse sometimes.

First try didn't work.  The `constructive-algebra` library has Gaussian Elimination, but it's not compatible with the current version of the `base` library.

```
rejecting: constructive-algebra-0.3.0 (conflict:
base==4.16.4.0/installed-4.16.4.0, constructive-algebra => base>=3 &&
<=4.3.1.0)
```

The `linearEqSolver` library has a package `Math.LinearEquationSolver`.  It might do the job, but the functions that do solving return an `IO`.  Weird.

Reddit user [NialNjae's solution](https://work.njae.me.uk/2024/01/04/advent-of-code-2023-day-24/) uses the `Linear.Matrix` package, which is limited to 4x4 matrices, but that's good enough for this problem.  Let's go with that.

## Building the matrix

Now we have the tools to build a matrix to solve.

```haskell
-- | For two of the three dimensions: build a matrix of coefficients, and the vector of constants for the RHS
buildMatrix :: (Hailstone3 -> Hailstone2) -> Problem -> (M44 Rational, V4 Rational)
buildMatrix flatten problem =
  (matrix, vector)
  where
    -- a list of for equations in the four rock variables: x, y, dx, dy
    -- (or diffent axes, depending on how it's getting flattened)
    equations = map (uncurry makeEquation) . take 4 . by2 . map flatten $ problem
    -- the matrix is everything but the right column
    matrix = v4FromList . map (v4FromList . dropEnd 1) $ equations
    -- the last column is the constants
    vector = v4FromList . map last $ equations
```

And the structure to solve twice, once in the X-Y plane, and once in the X-Z plane, which gives us all the values.

```haskell
part2 :: Problem -> Rational
part2 problem =
  x + y + z
  where
    (V4 x y _ _) = solve dropZ
    (V4 _ z _ _) = solve dropY
    solve :: (Hailstone3 -> Hailstone2) -> V4 Rational
    solve flatten =
      luSolveFinite matrix rhs
      where
        (matrix, rhs) = buildMatrix flatten problem
```
