{-# LANGUAGE TupleSections #-}

module Main where

import Advent (run)
import Data.List (tails)
import Data.List.Split (splitOn)
import Data.Tuple.Extra (both)
import Debug.Trace
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

main :: IO ()
main = run parse part1 part2

type TargetRange = (Integer, Integer)

type Point3 = V3 Integer

type Velocity3 = V3 Integer

type Hailstone3 = (Point3, Velocity3)

type Problem = (TargetRange, [Hailstone3])

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

part2 :: Problem -> Int
part2 = length

-- | Projects a 3D thing onto the x/y plane.
dropZ :: V3 a -> V2 a
dropZ (V3 x y _) = V2 x y

type Point2 = V2 Rational

type Velocity2 = V2 Rational

type Hailstone2 = (Point2, Velocity2)

-- | Projects a Hailstone onto the x/y plane
hailDropZ :: Hailstone3 -> Hailstone2
hailDropZ = both (dropZ . fmap fromIntegral)

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

-- | Is the given point on a hailstone's line in its future?
inFuture :: Hailstone2 -> Point2 -> Bool
inFuture (V2 hx _, V2 hvx _) (V2 x _) =
  signum hvx == signum (x - hx)

-- | Day 24, Part 1
part1 :: Problem -> Int
part1 (range, hailstones) =
  length . filter (intersectInFutureInRange range) . allPairs . map hailDropZ $ hailstones

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
