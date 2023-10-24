module Main (main) where

import System.Exit
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess

test1 :: Test
test1 = TestCase (assertEqual "one" 1 1)

tests = TestList [TestLabel "test1" test1]
