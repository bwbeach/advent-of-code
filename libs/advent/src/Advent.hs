module Advent (runTestAndInput) where

-- | Runs a solution for bath parts of one day against both test and input files.
runTestAndInput :: (Show b) => (String -> a) -> (a -> b) -> (a -> b) -> IO ()
runTestAndInput parse part1 part2 = do
  runFile "test.txt"
  runFile "input.txt"
  where
    runFile fileName = do
      putStrLn fileName
      text <- readFile fileName
      let input = parse text
      print . part1 $ input
      print . part2 $ input
