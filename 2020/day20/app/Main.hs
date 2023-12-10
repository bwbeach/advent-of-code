module Main where

import Advent
  ( only,
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

part1 :: Problem -> Integer
part1 tiles =
  product . unique . map tileNumber . filter isCornerTile $ all
  where
    isCornerTile t = isTopTile t && isLeftTile t
    isTopTile t = none (\x -> bottom x == top t && tileNumber t /= tileNumber x) all
    isLeftTile t = none (\x -> right x == left t && tileNumber t /= tileNumber x) all
    all = concatMap allOrientations tiles
    none f = not . any f

part2 :: Problem -> Int
part2 = length

mtsFromList :: (Ord a, Ord b) => [(a, b)] -> M.Map a (S.Set b)
mtsFromList =
  foldl' insert M.empty
  where
    insert m (a, b) = M.insert a (S.insert b (M.findWithDefault S.empty a m)) m

unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList