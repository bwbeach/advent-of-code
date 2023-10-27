{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Advent (gridEmpty, gridMap)
import Data.Map.Strict qualified as M
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess

testGridEmpty :: Test
testGridEmpty = TestCase (assertEqual "empty map" (gridMap gridEmpty) M.empty)

tests =
  TestList
    [ TestLabel "testGridEmpty" testGridEmpty
    ]
