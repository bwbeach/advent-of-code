module Main where

import Advent (run)
import qualified Data.Graph.Wrapper as G
import Data.List.Split (endBy, splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Tuple.Extra (second)

main :: IO ()
main = run parse part1 part2

type Color = String

type Rule = (Color, [(Int, Color)])

type Problem = [Rule]

parse :: String -> Problem
parse =
  map parseLine . endBy "\n"

-- | Parse one input line
-- Examples:
--    dotted black bags contain no other bags.
--    vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
parseLine :: String -> Rule
parseLine line =
  (bagColor left, parseRhs right)
  where
    parseRhs "no other bags." = []
    parseRhs s = map parseContent . splitOn "," $ s

    parseContent s =
      (read count, bagColor . unwords $ rest)
      where
        (count : rest) = words s

    [left, right] = splitOn " contain " line

    -- "bright red bags" -> "bright red"
    bagColor = unwords . take 2 . words

part1 :: Problem -> Int
part1 problem =
  length . filter (/= "shiny gold") . G.reachableVertices g $ "shiny gold"
  where
    -- Graph with colors as nodes, and edges going from a bag to the colors that holds it
    g = G.transpose . G.fromListSimple . map (second (map snd)) $ problem

-- | How many bags are inside a shiny gold bag?
-- ... one fewer than the total number of bags, including the shiny gold bag.
part2 :: Problem -> Int
part2 problem =
  bagCount (1, "shiny gold") - 1
  where
    -- How many total bags for n bags of color c?
    bagCount :: (Int, Color) -> Int
    bagCount (n, c) = n * (1 + sum (map bagCount (contentsOf c)))

    -- the contents of a bag of the given color
    contentsOf :: Color -> [(Int, Color)]
    contentsOf c = fromJust $ M.lookup c colorToContents

    -- a map from bag color to the contents of the bag
    colorToContents :: M.Map Color [(Int, Color)]
    colorToContents = M.fromList problem
