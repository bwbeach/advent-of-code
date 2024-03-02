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
part1 = sum . numbers

part2 :: Problem -> Int
part2 _ = 0

-- | All of the numbers in a JSON structure
--
-- Assumes that all numbers can be represented as in Int
numbers :: J.Value -> [Int]
numbers (J.Number n) = [fromJust . S.toBoundedInteger $ n]
numbers (J.Object km) = concatMap numbers . KM.elems $ km
numbers (J.Array v) = concatMap numbers . V.toList $ v
numbers (J.String _) = []
numbers (J.Bool _) = []
numbers J.Null = []
