# 2020 Day 20

## Reading the input

The input file is a list of tiles, separated by blank lines.  Each input tile looks like this, with the tile number on the first line, followed by the rows of the tile:

```
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###
```

Let's represent a tile with its tile number, and the strings that are the rows of characters:

```haskell
data Tile
  = Tile Integer [String]
  deriving (Eq, Show)
```

Parsing a tile is not too hard:

```haskell
parseTile :: String -> Tile
parseTile s =
  Tile number grid
  where
    number = read . dropEnd 1 . (!! 1) . words . head . lines $ s
    grid = tail . lines $ s
```

## Manipulating tiles

This representation for tiles makes flipping and rotating pretty easy.  Flipping a tile is just reversing the order of the rows:

```haskell
invert :: Tile -> Tile
invert (Tile n ss) = Tile n (reverse ss)
```

The build-in `transpose` function on lists flips a tile on the diagonal.  Combining that with the vertical flip of `invert` results in a 90-degree rotation.  We don't even need to figure out which direction it rotates, since we're going to use all rotations.

```haskell
rotate :: Tile -> Tile
rotate (Tile n ss) = Tile n (transpose . reverse $ ss)
```

These can be combined, and the rotations repeated, to produce all 8 possible orientations of a tile:

```haskell
allOrientations :: Tile -> [Tile]
allOrientations t = take 4 (iterate rotate t) ++ take 4 (iterate rotate (invert t))
```

These give the top and bottom edges of a tile, in left-to-right order:

```haskell
top :: Tile -> String
top (Tile _ ss) = head ss

bottom :: Tile -> String
bottom (Tile _ ss) = last ss
```

And these give the left and right edges, reading from top to bottom:

```haskell
left :: Tile -> String
left (Tile _ ss) = map head ss

right :: Tile -> String
right (Tile _ ss) = map last ss
```

## Part 1

To get the answer to part 1, we don't need to actually lay out all the tiles.  The problem description says:

    Tiles at the edge of the image also have this border, but the outermost edges won't line up with any other tiles.

  It's sufficient to find the four tiles that could be in the corners.

```haskell
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
```

