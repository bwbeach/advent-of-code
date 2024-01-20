module Main where

import Advent
  ( mtsFromList,
    only,
    run,
  )
import Data.List (foldl', transpose)
import Data.List.Extra (dropEnd)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = run parse part1 part2

data Tile
  = Tile Integer [String]
  deriving (Eq, Show)

tileNumber :: Tile -> Integer
tileNumber (Tile n _) = n

rotate :: Tile -> Tile
rotate (Tile n ss) = Tile n (transpose . reverse $ ss)

invert :: Tile -> Tile
invert (Tile n ss) = Tile n (reverse ss)

allOrientations :: Tile -> [Tile]
allOrientations t = take 4 (iterate rotate t) ++ take 4 (iterate rotate (invert t))

left :: Tile -> String
left (Tile _ ss) = map head ss

right :: Tile -> String
right (Tile _ ss) = map last ss

top :: Tile -> String
top (Tile _ ss) = head ss

bottom :: Tile -> String
bottom (Tile _ ss) = last ss

type TileMap = M.Map String (Integer, Tile)

type Problem = [Tile]

parse :: String -> Problem
parse = map parseTile . splitOn "\n\n"

parseTile :: String -> Tile
parseTile s =
  Tile number grid
  where
    number = read . dropEnd 1 . (!! 1) . words . head . lines $ s
    grid = tail . lines $ s

-- | Find the tiles in the four corners, and multiply their tile numbers.
--
-- This looks for all tiles that, in some orientation, could be in the top
-- left corner.  This is the same set that could be in *any* corner.  Each
-- such tile has two orientations where it can be in the corner, so we need
-- to pull out the unique tile numbers.
part1 :: Problem -> Integer
part1 tiles =
  product . unique . map tileNumber . filter isCornerTile $ all
  where
    -- A tile (in its orientation) can be at the top left
    isCornerTile t = isTopTile t && isLeftTile t
    -- A tile can be on the top row if its top side doesn't match the bottom side of any other tile
    isTopTile t = none (\x -> bottom x == top t && tileNumber t /= tileNumber x) all
    -- A tile can be on the left side if its left side doesn't match the right side of any other tile
    isLeftTile t = none (\x -> right x == left t && tileNumber t /= tileNumber x) all
    -- All tiles in all orientations
    all = concatMap allOrientations tiles
    -- Applying f to every item of a list is False
    none f = not . any f

part2 :: Problem -> Int
part2 = length

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList