module Main where

import Advent
  ( Grid (..),
    Point,
    gridGet,
    gridMap,
    gridParse,
    gridToList,
    neighbors,
    run,
  )
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse . replace '.' ' '

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)

part1 :: Problem -> Int
part1 g = sum . map (readPartNumber g) . findPartNumbers $ g

part2 :: Problem -> Int
part2 g =
  sum . mapMaybe starToMaybeRatio $ stars
  where
    -- Given a star location, return its gear ratio if it has two neighbors
    starToMaybeRatio p =
      if length neighboringNumbers == 2
        then Just (product neighboringNumbers)
        else Nothing
      where
        neighboringNumbers = starToNeigboringNumbers p

    -- Given the location of a star, return the part number values next to it.
    starToNeigboringNumbers p =
      map (readPartNumber g) . filter (any (`elem` starNeighbors)) $ partNumbers
      where
        starNeighbors = neighbors p

    -- All of the part numbers on the grid
    partNumbers = findPartNumbers g

    -- All of the points that have stars
    stars = map fst . filter ((== '*') . snd) . gridToList $ g

-- | Find all of the part numbers in the grid, each one as a list of points where the digits are
findPartNumbers :: Grid -> [[Point]]
findPartNumbers g =
  filter isPartNumber . map pointsOfNumber . filter isStartOfNumber $ points
  where
    -- All of the non-empty points in the grid
    points = M.keys . gridMap $ g

    -- Is the given point the start of a number?
    isStartOfNumber p = gridHasDigit p && not (gridHasDigit (left p))

    -- Does the grid have a digit at the given point?
    gridHasDigit p = isDigit . gridGet p $ g

    -- Given the starting point of a number, return all of the points that hold the number
    pointsOfNumber = takeWhile gridHasDigit . iterate right

    -- All of the neighbors of a number, and the number too (but that won't matter)
    neighborsOfNumber = S.toList . S.fromList . concatMap neighbors

    -- A number is a part number if it's next to a symbol
    isPartNumber = any (isSymbol . get) . neighborsOfNumber

    -- Symbols are any thing that's not empty or a digit
    isSymbol c = not (isDigit c || c == ' ')

    get = getGridPoint g

-- | Given the points that hold a number, return the number.
readPartNumber :: Grid -> [Point] -> Int
readPartNumber g = read . map (getGridPoint g)

getGridPoint :: Grid -> Point -> Char
getGridPoint g p = gridGet p g

left :: Point -> Point
left p = p + V2 (-1) 0

right :: Point -> Point
right p = p + V2 1 0