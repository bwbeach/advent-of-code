module Main where

import Advent (run)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.UTF8 as BSU
import Data.Maybe (fromJust)
import qualified Data.Scientific as S
import qualified Data.Vector as V

main :: IO ()
main = run parse part1 part2

type Problem = J.Value

parse :: String -> J.Value
parse = fromJust . J.decodeStrict . BSU.fromString

part1 :: Problem -> Int
part1 (J.Number n) = fromJust . S.toBoundedInteger $ n
part1 (J.Object km) = sum . map part1 . KM.elems $ km
part1 (J.Array v) = sum . map part1 . V.toList $ v
part1 (J.String _) = 0
part1 (J.Bool _) = 0
part1 J.Null = 0

part2 :: Problem -> Int
part2 _ = 0
