module Main where

import Advent (run)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.UTF8 as BSU
import Data.Maybe (fromJust)
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main = run parse part1 part2

type Problem = J.Value

-- | Turn the input file into a JSON AST
parse :: String -> J.Value
parse = fromJust . J.decodeStrict . BSU.fromString

part1 :: Problem -> Int
part1 = sum . numbers

part2 :: Problem -> Int
part2 = sum . numbers . filterJson (not . isRed)

-- | Is the value an object where one of the values is "red"?
isRed :: J.Value -> Bool
isRed (J.Object km) = (red `elem`) . KM.elems $ km
isRed _ = False

-- | The JSON value for "red"
red :: J.Value
red = J.String . T.pack $ "red"

-- | All of the numbers in a JSON structure
--
-- Assumes that all numbers can be represented as in Int
numbers :: J.Value -> [Int]
numbers (J.Number n) = [fromJust . S.toBoundedInteger $ n]
numbers (J.Object km) = concatMap numbers . KM.elems $ km
numbers (J.Array v) = concatMap numbers . V.toList $ v
numbers _ = []

-- | Filter an JSON Value, and everything it contains.
--
-- Any value not passing the test is replaced with Null.
-- Filtering is recursive for values that do pass the test.
filterJson :: (J.Value -> Bool) -> J.Value -> J.Value
filterJson test =
  go
  where
    go v = if test v then process v else J.Null

    process (J.Array v) = J.Array . V.map go $ v
    process (J.Object km) = J.Object . KM.map go $ km
    process x = x
