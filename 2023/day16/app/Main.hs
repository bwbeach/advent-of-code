{-# LANGUAGE TupleSections #-}

module Main where

import Advent
  ( Grid (..),
    Point,
    gridBounds,
    gridGet,
    gridMap,
    gridParse,
    run,
  )
import Data.Function ((&))
import Data.Graph.Wrapper
  ( Graph,
    fromListSimple,
    reachableVertices,
  )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Linear.V2 (V2 (..))

main :: IO ()
main = run parse part1 part2

part1 :: Problem -> Int
part1 problem =
  illuminated graph (E, V2 1 1)
  where
    graph = buildGraph problem

part2 :: Problem -> Int
part2 problem =
  maximum . map (illuminated graph) $ allStarts
  where
    allStarts = fromTop ++ fromBottom ++ fromLeft ++ fromRight
    fromTop = map (\x -> (S, V2 x y0)) [x0 .. x1]
    fromBottom = map (\x -> (N, V2 x y1)) [x0 .. x1]
    fromLeft = map (\y -> (E, V2 x0 y)) [y0 .. y1]
    fromRight = map (\y -> (W, V2 x1 y)) [y0 .. y1]
    (V2 x0 y0, V2 x1 y1) = gridBounds problem
    graph = buildGraph problem

type Problem = Grid

parse :: String -> Problem
parse = gridParse

data Dir
  = N
  | S
  | E
  | W
  deriving (Eq, Ord, Show)

-- | All four directions
fourDirections :: [Dir]
fourDirections = [N, S, E, W]

-- | A node in the graph: heading a given direction into a location in the grid.
-- Because the light is coming into the point given, that point is *energized*.
type Node = (Dir, Point)

-- | Given a starting place, how many tiles are illuminated?
illuminated :: Graph Node Node -> Node -> Int
illuminated graph start =
  S.size . S.fromList . map snd $ reachableVertices graph start

-- | Build a graph of Nodes from the grid of mirrors
buildGraph :: Grid -> Graph Node Node
buildGraph grid =
  -- start with the grid
  grid
    -- convert to graph nodes, four per cell in the grid, one per direction coming in
    & allNodes
    -- make pairs (node, [nextNode]) with the outgoing edges from each node
    & map nodeToEntry
    -- build a graph
    & fromListSimple
  where
    -- given a node, make a pair with the node and the destinations of outgoing edges
    nodeToEntry n = (n, nextNodes grid n)
    -- all of the nodes for a grid
    allNodes = concatMap pointToFourNodes . allPoints
    -- all of the cells in a grid
    allPoints = map fst . M.toList . gridMap
    -- convent one grid location into the four nodes for the four incoming directions
    pointToFourNodes p = map (,p) fourDirections

-- | Given one node in the graph, what are the next nodes.
nextNodes :: Grid -> Node -> [Node]
nextNodes g (d, p) =
  filter isInGrid . map next $ rule d (gridGet p g)
  where
    -- the next node, given an outgoing direction from this cell
    next d' = (d', p + dirDelta d')
    -- is the given node part of the grid?
    isInGrid (_, p') = gridGet p' g /= ' '

-- | The rules about what happens to a light beam coming in from a direction and hitting a mirror
rule :: Dir -> Char -> [Dir]
rule d '.' = [d]
rule N '/' = [E]
rule S '/' = [W]
rule E '/' = [N]
rule W '/' = [S]
rule N '\\' = [W]
rule S '\\' = [E]
rule E '\\' = [S]
rule W '\\' = [N]
rule N '-' = [E, W]
rule S '-' = [E, W]
rule E '-' = [E]
rule W '-' = [W]
rule N '|' = [N]
rule S '|' = [S]
rule E '|' = [N, S]
rule W '|' = [N, S]

-- | What do the directions mean on the grid?
dirDelta :: Dir -> Point
dirDelta N = V2 0 (-1)
dirDelta S = V2 0 1
dirDelta E = V2 1 0
dirDelta W = V2 (-1) 0
