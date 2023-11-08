{-# LANGUAGE ImportQualifiedPost #-}

module Advent
  ( Grid (..),
    countThings,
    gridBounds,
    gridEmpty,
    gridFormat,
    gridMap,
    gridParse,
    memoize,
    runMemoize,
    only,
    run,
    runTestAndInput,
  )
where

import Control.Monad.State.Lazy
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Linear.V2 (V2 (..))

-- | Runs a solution on a list of input files.
run :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> [String] -> IO ()
run _ _ _ [] = pure ()
run parse part1 part2 (f : fs) = do
  runFile f
  run parse part1 part2 fs
  where
    runFile fileName = do
      putStrLn fileName
      text <- readFile fileName
      let input = parse text
      print . part1 $ input
      print . part2 $ input

-- | Runs a solution for bath parts of one day against both test and input files.
runTestAndInput :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> IO ()
runTestAndInput parse part1 part2 = do
  run parse part1 part2 ["test.txt", "input.txt"]

-- | Takes a Foldable sequence of things and counts them.
-- Returns a map from a thing in the sequence to the number of occurrences.
countThings :: (Ord k, Foldable f) => f k -> M.Map k Int
countThings =
  foldl incr M.empty
  where
    incr m k = M.insert k (1 + M.findWithDefault 0 k m) m

-- | Returns the contents if a singleton list.
--
-- It's an error if the length of the list is not 1.
only :: (Show a) => [a] -> a
only [a] = a
only as = error ("expected exactly one: " ++ show as)

-- | A grid of characters.
--
-- Advent of Code problems frequently start as grids of characters.
-- The problems usualy index them 1-based, with (1, 1) at the upper
-- left, positive x going right, and positive y going down.
--
-- A grid is stored as a map from position to non-space character at
-- that position.
newtype Grid = Grid (M.Map (V2 Int) Char) deriving (Eq, Show, Read)

-- | An empty grid, with no cells filled.
gridEmpty :: Grid
gridEmpty = Grid M.empty

-- | Extracts the map from a Grid
gridMap :: Grid -> M.Map (V2 Int) Char
gridMap (Grid m) = m

-- | Bounds of a grid: upper left corner and lower right corner
gridBounds :: Grid -> (V2 Int, V2 Int)
gridBounds (Grid m) =
  (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  where
    points = M.keys m
    xs = [x | (V2 x _) <- points]
    ys = [y | (V2 _ y) <- points]

-- | Grids are read one row per line, ignoring spaces.
gridParse :: String -> Grid
gridParse =
  Grid . M.fromList . concat . zipWith parseLine [1 ..] . lines
  where
    parseLine y = concat . zipWith (parseOne y) [1 ..]

    parseOne y x ' ' = []
    parseOne y x c = [(V2 x y, c)]

-- | Formats a grid as a String
gridFormat :: Grid -> String
gridFormat (Grid m) =
  unlines $ map lineFormat [y0 .. y1]
  where
    (V2 x0 y0, V2 x1 y1) = gridBounds (Grid m)
    lineFormat y = map (cellFormat y) [x0 .. x1]
    cellFormat y x = fromMaybe ' ' $ M.lookup (V2 x y) m

-- | The memoization state that stores the already-computed answers.
type Memo k v = M.Map k v

-- | Looks to see if an answer is in the memo.
memoLookup :: (Ord k) => k -> State (Memo k v) (Maybe v)
memoLookup k =
  state (\ms -> (M.lookup k ms, ms))

-- | Store an answer in the memo.
memoInsert :: (Ord k) => k -> v -> State (Memo k v) ()
memoInsert k v =
  state (\ms -> ((), M.insert k v ms))

-- | Wrap a monad and don't call it if we already have the answer.
-- If we do call it, insert the answer in the state.
memoize :: (Ord k) => (k -> State (Memo k v) v) -> k -> State (Memo k v) v
memoize f n = do
  maybeResult <- memoLookup n
  case maybeResult of
    Nothing -> do
      result <- f n
      memoInsert n result
      return result
    Just result -> do
      return result

-- | Run a memoized function.
runMemoize :: (Ord k) => (k -> State (Memo k v) v) -> k -> v
runMemoize m x =
  evalState (m x) M.empty
