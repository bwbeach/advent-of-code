module Main where

import Advent (run)
import Data.List.Extra (dropEnd)
import Data.List.Split (splitOn)
import Linear.Matrix (M44 (..), luSolveFinite)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.V4 (V4 (..))

main :: IO ()
main = run parse part1 part2

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

-- | Does the equation for the two test hailstones work in the XZ plane?
--
-- >>> tryEquationYZ hail1 hail2
-- (48 % 1,48 % 1)
tryEquationYZ :: Hailstone3 -> Hailstone3 -> (Rational, Rational)
tryEquationYZ h1 h2 =
  (a * y + b * z + c * dy + d * dz, e)
  where
    [a, b, c, d, e] = makeEquation (dropX h1) (dropX h2)
    (V2 y z, V2 dy dz) = dropX aRock

-- | Take things two-by-two from a list
--
-- >>> by2 [1, 2, 3, 4, 5, 6, 7]
-- [(1,2),(3,4),(5,6)]
by2 :: [a] -> [(a, a)]
by2 (a : b : cs) = (a, b) : by2 cs
by2 _ = []

-- | Make a V4 from a list
v4FromList :: (Show a) => [a] -> V4 a
v4FromList [a, b, c, d] = V4 a b c d
v4FromList x = error ("Not four in list: " ++ show x)

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

part1 :: Problem -> Int
part1 = length
