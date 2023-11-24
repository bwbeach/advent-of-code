module Main where

import Advent
  ( Point,
    gridMap,
    gridParse,
    run,
  )
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Tuple.Extra (first)
import Debug.Trace
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

main :: IO ()
main = run parse part1 part2

type Point3 = V3 Int

point3From2 :: Point -> Point3
point3From2 (V2 x y) = V3 x y 0

point3X :: V3 a -> a
point3X (V3 x _ _) = x

point3Y :: V3 a -> a
point3Y (V3 _ y _) = y

point3Z :: V3 a -> a
point3Z (V3 _ _ z) = z

point3Neighbors :: (Num a, Eq a) => V3 a -> [V3 a]
point3Neighbors p =
  [ p + V3 dx dy dz
    | dx <- [-1, 0, 1],
      dy <- [-1, 0, 1],
      dz <- [-1, 0, 1],
      dx /= 0 || dy /= 0 || dz /= 0
  ]

newtype Grid3 = Grid3 (M.Map Point3 Char)

grid3Map :: Grid3 -> M.Map Point3 Char
grid3Map (Grid3 m) = m

grid3Get :: V3 Int -> Grid3 -> Maybe Char
grid3Get p (Grid3 m) = M.lookup p m

grid3Points :: Grid3 -> [Point3]
grid3Points (Grid3 m) = M.keys m

grid3Bounds :: Grid3 -> (Point3, Point3)
grid3Bounds (Grid3 g) =
  (V3 (minimum xs) (minimum ys) (minimum zs), V3 (maximum xs) (maximum ys) (maximum zs))
  where
    ps = M.keys g
    xs = map point3X ps
    ys = map point3Y ps
    zs = map point3Z ps

grid3Format :: Grid3 -> String
grid3Format (Grid3 g) =
  (++ "\n--------\n\n") . intercalate "\n" . map formatSlice $ [z0 .. z1]
  where
    (V3 x0 y0 z0, V3 x1 y1 z1) = grid3Bounds (Grid3 g)
    formatSlice z = unlines . map (formatRow z) $ [y0 .. y1]
    formatRow z y = map (formatCell z y) [x0 .. x1]
    formatCell z y x = fromMaybe '.' (M.lookup (V3 x y z) g)

type Problem = Grid3

parse :: String -> Problem
parse =
  Grid3 . M.fromList . map (first point3From2) . M.toList . gridMap . gridParse . replace '.' ' '
  where
    replace a b = map (\x -> if x == a then b else x)

part1 :: Problem -> Int
part1 = length . M.toList . grid3Map . (!! 6) . conway3

conway3 :: Grid3 -> [Grid3]
conway3 = iterate step3

step3 :: Grid3 -> Grid3
step3 g =
  Grid3 . M.fromList $ result
  where
    result =
      [ (p, c')
        | p <- candidates g,
          let c = get p,
          let ns = mapMaybe get . point3Neighbors $ p,
          c' <- maybeToList $ rule c ns
      ]
    candidates = uniqueValues . concatMap selfAndNeighbors . grid3Points
    get :: Point3 -> Maybe Char
    get p = grid3Get p g
    uniqueValues = S.toList . S.fromList
    selfAndNeighbors p = p : point3Neighbors p

rule :: Maybe Char -> [Char] -> Maybe Char
rule (Just '#') "##" = Just '#'
rule (Just '#') "###" = Just '#'
rule Nothing "###" = Just '#'
rule _ _ = Nothing

part2 :: Problem -> Int
part2 = length . M.toList . grid3Map
