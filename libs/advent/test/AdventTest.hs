{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Advent (Grid (..), gridEmpty, gridFormat, gridMap, gridParse)
import Data.Map.Strict qualified as M
import Linear.V2 (V2 (..))
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess

testGridEmpty :: Test
testGridEmpty = TestCase (assertEqual "empty map" (gridMap gridEmpty) M.empty)

testGridParseFormat :: Test
testGridParseFormat =
  TestCase (assertEqual "small grid" reformatted original)
  where
    reformatted = gridFormat grid
    grid = gridParse original
    original = "a \n b\n"

tests =
  TestList
    [ TestLabel "testGridEmpty" testGridEmpty,
      TestLabel "testGridParse" testGridParseFormat
    ]
