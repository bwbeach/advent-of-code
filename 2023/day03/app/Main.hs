module Main where

import Advent (Grid (..), Point, gridGet, gridMap, gridParse, run)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, mapMaybe)
import Linear.V2 (V2 (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = run parse part1 part2

type Problem = Grid

parse :: String -> Problem
parse = gridParse . replace '.' ' ' 

replace :: Eq a => a -> a -> [a] -> [a] 
replace a b = map (\x -> if x == a then b else x)

part1 :: Problem -> Int
part1 g = sum . map (readPartNumber g) . findPartNumbers $ g

part2 :: Problem -> Int
part2 g = 
    sum . mapMaybe starToMaybeRatio $ stars
    where 
        starToMaybeRatio p = 
            if length neighboringNumbers == 2 
                then Just (product neighboringNumbers)
                else Nothing
            where 
                neighboringNumbers = starToNeigboringNumbers p
        starToNeigboringNumbers p = 
            map (readPartNumber g) . filter (any (`elem` starNeighbors)) $ partNumbers
            where 
                starNeighbors = neighbors p
        partNumbers = findPartNumbers g
        stars = filter ((== '*') . getGridPoint g) points 
        points = M.keys . gridMap $ g


findPartNumbers :: Grid -> [[Point]]
findPartNumbers g =
    filter isPartNumber . map pointsOfNumber . filter isStartOfNumber $ points 
    where
        points = M.keys . gridMap $ g
        isStartOfNumber p = gridHasDigit p && not (gridHasDigit (left p))
        gridHasDigit p = isDigit . gridGet p  $ g
        pointsOfNumber = takeWhile gridHasDigit . iterate right
        isPartNumber = any (isSymbol . get) . neighborsOfNumber
        isSymbol c = not (isDigit c || c == ' ')
        get = getGridPoint g
        neighborsOfNumber = S.toList . S.fromList . concatMap neighbors

readPartNumber :: Grid -> [Point] -> Int 
readPartNumber g = read . map (getGridPoint g)

getGridPoint :: Grid -> Point -> Char 
getGridPoint g p = gridGet p g

neighbors :: Point -> [Point]
neighbors p = 
    [ p + V2 dx dy |
      dx <- [(-1) .. 1],
      dy <- [(-1) .. 1]
    ]

left :: Point -> Point
left p = p + V2 (-1) 0

right :: Point -> Point
right p = p + V2 1 0