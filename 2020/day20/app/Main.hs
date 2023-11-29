module Main where

import Advent
  ( Grid (..),
    gridParse,
    only,
    run,
  )
import Data.List.Extra (dropEnd)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main = run parse part1 part2

type Problem = M.Map Integer Grid

parse :: String -> Problem
parse = M.fromList . map parseTile . splitOn "\n\n"

parseTile :: String -> (Integer, Grid)
parseTile s =
  (number, grid)
  where
    number = read . dropEnd 1 . (!! 1) . words . head . lines $ s
    grid = gridParse . unlines . tail . lines $ s

part1 :: Problem -> Int
part1 = M.size

part2 :: Problem -> Int
part2 = length
