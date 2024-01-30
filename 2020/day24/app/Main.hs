module Main where

import Advent (Point, run)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

data Direction =
     E | SE | SW | W | NW | NE 
     deriving (Show)

-- | What colors a tile can show 
data Color = White | Black deriving (Eq, Show) 

flipTile :: Color -> Color
flipTile White = Black
flipTile Black = White

-- | A vector in hexagon space. 
-- The space is mapped two an (x, y) space, with positive x going E
-- and positive y going NW. 
type Vector = V2 Int 

-- | The path described by one input line
type Path = [Vector]

type Problem = [Path]

parse :: String -> Problem
parse = map parseLine . lines

-- | Convert one input line into a Path 
--
-- >>> parseLine "eswnwe"
parseLine :: String -> Path
parseLine =
    map dirToVec . go
    where
        go [] = []
        go ('e' : cs) = E : go cs
        go ('s' : 'e' : cs) = SE : go cs 
        go ('s' : 'w' : cs) = SW : go cs
        go ('w' : cs) = W : go cs
        go ('n' : 'w' : cs) = NW : go cs
        go ('n' : 'e' : cs) = NE : go cs
        go s = error ("bad direction: " ++ s)

part1 :: Problem -> Int
part1 problem = 
    length . filter (== Black) . map snd . M.toList $ result 
    where 
        result = foldl' addOne M.empty locations
        addOne m loc = M.insert loc (flipTile (M.findWithDefault White loc m)) m
        locations = map sum problem

part2 :: Problem -> Int
part2 = length

-- | Convert a Direction into a two-dimensional vector
dirToVec :: Direction -> Point 
dirToVec E = V2 1 0
dirToVec SE = V2 0 (-1)
dirToVec SW = V2 (-1) (-1)
dirToVec W = V2 (-1) 0
dirToVec NW = V2 0 1
dirToVec NE = V2 1 1
