module Main where

import Advent
  ( Grid,
    Point,
    gridGet,
    gridParse,
    gridToList,
    run,
  )
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Linear.V2 (V2 (..))
import Tile
  ( Tile (..),
    allOrientations,
    assembleTiles,
    fitsAbove,
    fitsLeftOf,
    parseTile,
    tileNumber,
    tileRows,
    trim,
  )

main :: IO ()
main = run parse part1 part2

type Problem = [Tile]

parse :: String -> Problem
parse = map parseTile . splitOn "\n\n"

-- | Find the tiles in the four corners, and multiply their tile numbers.
--
-- This looks for all tiles that, in some orientation, could be in the top
-- left corner.  This is the same set that could be in *any* corner.  Each
-- such tile has two orientations where it can be in the corner, so we need
-- to pull out the unique tile numbers.
part1 :: Problem -> Integer
part1 tiles =
  product . unique . map tileNumber . cornerTiles $ allTiles
  where
    -- All tiles in all orientations
    allTiles = concatMap allOrientations tiles

-- | All of the tilecs/orientations that go in a corner.
-- This includes two orientations for each tile that goes in a corner,
-- only one of which will be used once the whole thing is assembled.
cornerTiles :: [Tile] -> [Tile]
cornerTiles allTiles =
  filter isCornerTile allTiles
  where
    -- A tile (in its orientation) can be at the top left
    isCornerTile t = isTopTile t && isLeftTile t
    -- A tile can be on the top row if its top side doesn't match the bottom side of any other tile
    isTopTile t = none (`fitsAbove` t) allTiles
    -- A tile can be on the left side if its left side doesn't match the right side of any other tile
    isLeftTile t = none (`fitsLeftOf` t) allTiles
    -- Applying f to every item of a list is False
    none f = not . any f

part2 :: Problem -> Int
part2 =
  minimum . map (countNonMonsters . gridFromTile) . allOrientations . buildBigTile
  where
    gridFromTile = gridParse . unlines . tileRows

-- | How many of the hashes ('#') in a grid are not sea monsters?
countNonMonsters :: Grid -> Int
countNonMonsters g =
  length allHashes - length (seaMonsterPositions g)
  where
    allHashes = filter (== '#') . map snd . gridToList $ g

-- | Fit all of the input tiles together into one big tile
buildBigTile :: Problem -> Tile
buildBigTile tiles =
  assembleTiles . map (map trim) $ rowsOfTiles
  where
    -- The rows of tiles
    rowsOfTiles = map (iterateMaybe nextTileRight) leftEdge
    -- The tiles of the left edge
    leftEdge = iterateMaybe nextTileDown topLeft
    -- What is the next tile below the given one?
    nextTileDown t = listOfZeroOrOneToMaybe . filter (t `fitsAbove`) $ allTiles
    -- What is the next tile to the right of the given one?
    nextTileRight t = listOfZeroOrOneToMaybe . filter (t `fitsLeftOf`) $ allTiles
    -- A tile that can be in the top-left corner
    topLeft = head . cornerTiles $ allTiles
    -- All tiles in all orientations
    allTiles = concatMap allOrientations tiles

-- | Convert a list containing zero or one item into a Maybe
-- listToMaybe would work.  I like this better because it will complain
-- if there's more than one item in the list.
listOfZeroOrOneToMaybe :: (Show a) => [a] -> Maybe a
listOfZeroOrOneToMaybe [] = Nothing
listOfZeroOrOneToMaybe [a] = Just a
listOfZeroOrOneToMaybe x = error ("Expected zero or one: " ++ show x)

-- | Like iterate, but stops when the function returns Nothing.
--
-- >>> iterateMaybe (\x -> if x < 10 then Just (x + 1) else Nothing) 0
-- [0,1,2,3,4,5,6,7,8,9,10]
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a =
  a : go a
  where
    go b =
      case f b of
        Just b' -> b' : go b'
        Nothing -> []

-- | All of the unique values in a list
--
-- >>> unique [1, 1, 2, 3, 1, 2, 3, 4]
-- [1,2,3,4]
unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

-- | Return all of the positions in a grid that are sea monster.
seaMonsterPositions :: Grid -> [Point]
seaMonsterPositions g =
  unique . concatMap (seaMonsterAt g) $ allPoints
  where
    -- All of the points in the grid
    allPoints = map fst . gridToList $ g

-- | Sea monster locations for any sea monsters at point p in g
seaMonsterAt :: Grid -> Point -> [Point]
seaMonsterAt g p =
  if all isHash monster
    then monster
    else []
  where
    monster = map (+ p) seaMonsterOffsets
    isHash q = gridGet q g == '#'

-- | Positions of a sea monster, relative to the upper-left corner.
--
-- >>> take 10 seaMonsterOffsets
-- [V2 0 1,V2 1 2,V2 4 2,V2 5 1,V2 6 1,V2 7 2,V2 10 2,V2 11 1,V2 12 1,V2 13 2]
seaMonsterOffsets :: [Point]
seaMonsterOffsets =
  map ((+ V2 (-1) (-1)) . fst) . filter ((== '#') . snd) . gridToList $ g
  where
    g =
      gridParse . unlines $
        [ "                  # ",
          "#    ##    ##    ###",
          " #  #  #  #  #  #   "
        ]
