{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Advent
  ( Grid (..),
    countThings,
    gridBounds,
    gridEmpty,
    gridFormat,
    gridMap,
    gridParse,
  )
import Data.Map.Strict qualified as M
import Linear.V2 (V2 (..))
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess

testCountThings :: Test
testCountThings = TestCase (assertEqual "countThings" (M.fromList [('a', 2), ('l', 1), ('f', 1)]) (countThings "alfa"))

testGridEmpty :: Test
testGridEmpty = TestCase (assertEqual "empty map" (gridMap gridEmpty) M.empty)

testGridBounds :: Test
testGridBounds =
  TestCase (assertEqual "bounds" (V2 1 1, V2 3 2) (gridBounds . gridParse $ "a b\nc"))

testGridParseFormat :: Test
testGridParseFormat =
  TestCase (assertEqual "small grid" reformatted original)
  where
    reformatted = gridFormat grid
    grid = gridParse original
    original = "ab\n d\n"

tests =
  TestList
    [ TestLabel "testGridEmpty" testGridEmpty,
      TestLabel "testGridBounds" testGridBounds,
      TestLabel "testGridParse" testGridParseFormat
    ]
