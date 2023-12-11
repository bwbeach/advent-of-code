{-# LANGUAGE TupleSections #-}

module Main where

import Advent
  ( Grid,
    Point,
    gridMap,
    gridParse,
    run,
  )
import Data.List (transpose)
import qualified Data.Map.Strict as M
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

type Problem = [String]

parse :: String -> Problem
parse = lines

part1 :: Problem -> Int
part1 = sumDistances . gridParse . unlines . expand

expand :: Problem -> Problem
expand =
  transpose . expandV . transpose . expandV
  where
    expandV = concatMap expandOne
    expandOne s =
      if all (== '.') s
        then [s, s]
        else [s]

sumDistances :: Grid -> Int
sumDistances = sum . map (uncurry manhattan) . allPairs . map fst . filter ((/= '.') . snd) . M.toList . gridMap

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs [_] = []
allPairs (a : as) = map (a,) as ++ allPairs as

manhattan :: Point -> Point -> Int
manhattan (V2 x1 y1) (V2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

part2 :: Problem -> Int
part2 = length
