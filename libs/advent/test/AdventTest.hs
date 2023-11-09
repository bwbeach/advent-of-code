{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Advent
  ( Grid (..),
    countThings,
    gridBounds,
    gridEmpty,
    gridFormat,
    gridGet,
    gridMap,
    gridParse,
    gridSet,
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

testGridSet :: Test
testGridSet =
  TestCase (assertEqual "gridSet" (M.fromList [(V2 1 1, 'b')]) (gridMap g))
  where
    g = gridSet (V2 2 2) ' ' . gridSet (V2 1 1) 'b' . gridSet (V2 2 2) 'a' $ gridEmpty

testGridGet :: Test
testGridGet =
  TestCase (assertEqual "gridGet" "a " [gridGet (V2 1 1) g, gridGet (V2 2 2) g])
  where
    g = gridParse "a"

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
    [ TestLabel "testCountThings" testCountThings,
      TestLabel "testGridSet" testGridSet,
      TestLabel "testGridEmpty" testGridEmpty,
      TestLabel "testGridBounds" testGridBounds,
      TestLabel "testGridParse" testGridParseFormat
    ]
