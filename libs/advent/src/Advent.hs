{-# LANGUAGE ImportQualifiedPost #-}

module Advent
  ( Grid (..),
    gridEmpty,
    gridMap,
    gridParse,
    runTestAndInput,
  )
where

import Data.Map.Strict qualified as M
import Linear.V2 (V2 (..))

-- | Runs a solution for bath parts of one day against both test and input files.
runTestAndInput :: (Show b) => (String -> a) -> (a -> b) -> (a -> b) -> IO ()
runTestAndInput parse part1 part2 = do
  runFile "test.txt"
  runFile "input.txt"
  where
    runFile fileName = do
      putStrLn fileName
      text <- readFile fileName
      let input = parse text
      print . part1 $ input
      print . part2 $ input

-- | A grid of characters.
--
-- Advent of Code problems frequently start as grids of characters.
-- The problems usualy index them 1-based, with (1, 1) at the upper
-- left, positive x going right, and positive y going down.
--
-- A grid is stored as a map from position to non-space character at
-- that position.
newtype Grid = Grid (M.Map (V2 Int) Char) deriving (Eq, Show, Read)

-- | An empty grid, with no cells filled.
gridEmpty :: Grid
gridEmpty = Grid M.empty

-- | Extracts the map from a Grid
gridMap :: Grid -> M.Map (V2 Int) Char
gridMap (Grid m) = m

-- | Grids are read one row per line, ignoring spaces.
gridParse :: String -> Grid
gridParse =
  Grid . M.fromList . concat . zipWith parseLine [1 ..] . lines
  where
    parseLine y line = concat $ zipWith (parseOne y) [1 ..] line

    parseOne y x ' ' = []
    parseOne x y c = [(V2 x y, c)]

-- instance Show Grid where
--  shows g _ = "g"

-- | Converts lines of characters to a grid.
--
-- The grid is represented as a map from (V2 x y) to a non-space
-- character at that position.
-- instance Read Grid where
