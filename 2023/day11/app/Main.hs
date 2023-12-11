{-# LANGUAGE TupleSections #-}

module Main where

import Advent
  ( Grid,
    Point,
    gridBounds,
    gridMap,
    gridParse,
    run,
  )
import Data.Bits (Bits (xor))
import Data.List (transpose)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
part2 problem =
  sum . map (uncurry manhattan) . allPairs . map expandPoint $ galaxies
  where
    expandPoint (V2 x y) = V2 (expandX x) (expandY y)
    expandX x = x + (1000000 - 1) * (length . filter (< x) $ emptyCols)
    expandY y = y + (1000000 - 1) * (length . filter (< y) $ emptyRows)
    emptyCols = S.toList $ S.difference (S.fromList [x0 .. x1]) (S.fromList . map pointX $ galaxies)
    emptyRows = S.toList $ S.difference (S.fromList [y0 .. y1]) (S.fromList . map pointY $ galaxies)
    (V2 x0 y0, V2 x1 y1) = gridBounds grid
    galaxies = map fst . filter ((/= '.') . snd) . M.toList . gridMap $ grid
    grid = gridParse . unlines $ problem
    pointX (V2 x _) = x
    pointY (V2 _ y) = y
