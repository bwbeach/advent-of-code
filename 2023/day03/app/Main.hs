module Main where

import Advent (Grid (..), Point, gridGet, gridMap, gridParse, run)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
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
part1 g =
     sum . map readPartNumber . filter isPartNumber . map pointsOfNumber . filter isStartOfNumber $ points
     where 
        points = M.keys . gridMap $ g

        readPartNumber :: [Point] -> Int 
        readPartNumber = read . map get

        isPartNumber :: [Point] -> Bool
        isPartNumber = any (isSymbol . get) . neighborsOfNumber

        isSymbol c = not (isDigit c || c == ' ')

        get p = gridGet p g

        neighborsOfNumber :: [Point] -> [Point]
        neighborsOfNumber = S.toList . S.fromList . concatMap neighbors
            
        pointsOfNumber = takeWhile gridHasDigit . iterate right

        isStartOfNumber :: Point -> Bool 
        isStartOfNumber p = gridHasDigit p && not (gridHasDigit (left p))

        gridHasDigit p = isDigit . gridGet p  $ g

        left p = p + V2 (-1) 0
        right p = p + V2 1 0



part2 :: Problem -> Int
part2 = length . M.toList . gridMap

neighbors :: Point -> [Point]
neighbors p = 
    [ p + V2 dx dy |
      dx <- [(-1) .. 1],
      dy <- [(-1) .. 1]
    ]
