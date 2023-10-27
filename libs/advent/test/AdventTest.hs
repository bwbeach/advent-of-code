{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Advent (Grid (..), gridEmpty, gridMap, gridParse)
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

testGridParse :: Test
testGridParse =
  TestCase (assertEqual "small grid" expectedGrid actualGrid)
  where
    expectedGrid = Grid . M.fromList $ [(V2 1 1, 'a'), (V2 2 2, 'b')]
    actualGrid = gridParse "a \n b\n"

tests =
  TestList
    [ TestLabel "testGridEmpty" testGridEmpty,
      TestLabel "testGridParse" testGridParse
    ]
