module Tile
  ( Tile (..),
    allOrientations,
    assembleTiles,
    fitsAbove,
    fitsLeftOf,
    invert,
    parseTile,
    rotate,
    tileNumber,
    tileRows,
    trim,
  )
where

import Data.List (transpose)
import Data.List.Extra (dropEnd)

-- | One tile
-- Used for tiles in the input problem, and the bigger tile resulting from assembling them.
data Tile
  = Tile
      Integer -- The tile number
      [String] -- The rows of characters saying what's in the tile
  deriving (Eq, Show)

-- | The number of the tile
tileNumber :: Tile -> Integer
tileNumber (Tile n _) = n

-- | The rows of tile contents
tileRows :: Tile -> [String]
tileRows (Tile _ rs) = rs

-- | Parse one tile from the problem input.
--
-- >>> parseTile "Tile 57:\nabc\ndef\nghi\n"
-- Tile 57 ["abc","def","ghi"]
parseTile :: String -> Tile
parseTile s =
  Tile number grid
  where
    number = read . dropEnd 1 . (!! 1) . words . head . lines $ s
    grid = tail . lines $ s

-- | Rotate a tile 90 degrees clockwise.
--
-- >>> rotate (Tile 1 ["abc", "def"])
-- Tile 1 ["da","eb","fc"]
rotate :: Tile -> Tile
rotate (Tile n ss) = Tile n (transpose . reverse $ ss)

-- | Flip a tile vertically: the top becomes the bottom, and vice versa.
-- The left is still on the left, and the right is still on the right.
--
-- >>> invert (Tile 1 ["abc", "def"])
-- Tile 1 ["def","abc"]
invert :: Tile -> Tile
invert (Tile n ss) = Tile n (reverse ss)

-- | Trim the outer edge off a tile
--
-- >>> trim (Tile 1 ["abcde", "fghij", "klmno", "pqrst"])
-- Tile 1 ["ghi","lmn"]
trim :: Tile -> Tile
trim (Tile n ss) =
  Tile n (dropFirstAndLast . map dropFirstAndLast $ ss)
  where
    dropFirstAndLast = drop 1 . dropEnd 1

-- | The left edge of a tile, reading from top to bottom
--
-- >>> left (Tile 1 ["abc", "def", "ghi"])
-- "adg"
left :: Tile -> String
left (Tile _ ss) = map head ss

-- | The right edge of a tile, reading from top to bottom
--
-- >>> right (Tile 1 ["abc", "def", "ghi"])
-- "cfi"
right :: Tile -> String
right (Tile _ ss) = map last ss

-- | The top edge of a tile, reading from left to right
--
-- >>> top (Tile 1 ["abc", "def", "ghi"])
-- "abc"
top :: Tile -> String
top (Tile _ ss) = head ss

-- | The bottom edge of a tile, reading from left to right
--
-- >>> top (Tile 1 ["abc", "def", "ghi"])
-- "abc"
bottom :: Tile -> String
bottom (Tile _ ss) = last ss

-- | All of the possible orientations of a tile
--
-- There are four ways it can be rotated, and after it's flipped
-- over, there are four more.
allOrientations :: Tile -> [Tile]
allOrientations t = take 4 (iterate rotate t) ++ take 4 (iterate rotate (invert t))

-- | Does tile `a` fit just above tile `b`?
fitsAbove :: Tile -> Tile -> Bool
fitsAbove a b = bottom a == top b && tileNumber a /= tileNumber b

-- | Does tile `a` fit on the left side of tile `b`?
fitsLeftOf :: Tile -> Tile -> Bool
fitsLeftOf a b = right a == left b && tileNumber a /= tileNumber b

-- | Combine a grid of tiles into one big tile
--
-- >>> assembleTiles [[Tile 1 ["abcd", "hijk", "nopq", "uvwx"], Tile 2 ["defg", "klmn", "qrst", "xyz0"]], [Tile 3 ["uvwx", "1234", "890A"], Tile 4 ["xyz0", "4567", "ABCD"]]]
-- Tile 0 ["abcddefg","hijkklmn","nopqqrst","uvwxxyz0","uvwxxyz0","12344567","890AABCD"]
assembleTiles :: [[Tile]] -> Tile
assembleTiles rows =
  Tile 0 (concatMap assembleRow rows)
  where
    -- concatenate the strings across rows of a row a tiles
    assembleRow = map concat . transpose . map tileRows
