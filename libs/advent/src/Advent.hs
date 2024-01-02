{-# LANGUAGE ImportQualifiedPost #-}

module Advent
  ( LifeGrid (lgNew, lgGet, lgCells, lgCandidates),
    Point (..),
    Rectangle (..),
    rectangleContains,
    rectanglePoints,
    Grid (..),
    countThings,
    gridBounds,
    gridEmpty,
    gridFormat,
    gridFromList,
    gridGet,
    gridSet,
    gridMap,
    gridParse,
    gridToList,
    life,
    memoize,
    neighbors,
    neighborsAndSelf,
    runMemoize,
    only,
    replace,
    run,
  )
where

import Control.Monad.State.Lazy
  ( MonadState (state),
    State,
    evalState,
  )
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import GHC.TypeLits qualified as Ints
import Linear.V2 (V2 (..))
import System.Exit (exitFailure, exitSuccess)

{-
  Each problem appears in its own directory.  There's a cabal project
  named for the day, such as "day08", and also these files:

     answers.txt - a list of the inputs and their expected answers
                   (see parseAnswerLine for details of the format)

     test.txt - the sample input from the problem description

     input.txt - the actual input
-}

-- | Runs a solution on a list of input files listed in answers.txt.
run :: (Eq b, Eq c, Read b, Read c, Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> IO ()
run parse part1 part2 = do
  answers <- readAnswers
  oks <- mapM runOne answers
  if and oks
    then exitSuccess
    else exitFailure
  where
    runOne (fileName, (mb, mc)) = do
      text <- readFile fileName
      let input = parse text
      let b = part1 input
      let c = part2 input
      ok1 <- checkAnswer "part1" mb b
      ok2 <- checkAnswer "part2" mc c
      putStrLn (fileName ++ " " ++ show b ++ " " ++ show c)
      return (ok1 && ok2)

-- | Checks one answer
checkAnswer :: (Eq a, Show a) => [Char] -> Maybe a -> a -> IO Bool
checkAnswer title expected actual = do
  case expected of
    Nothing -> do
      putStrLn (title ++ " UNCHECKED: " ++ show actual)
      return True
    Just e ->
      if e == actual
        then return True
        else do
          putStrLn (title ++ ":  EXPECTED " ++ show e ++ " GOT " ++ show actual)
          return False

-- | Reads expected answers
readAnswers :: (Read b, Read c) => IO [(String, (Maybe b, Maybe c))]
readAnswers = do
  answers <- readFile "answers.txt"
  let items = map parseAnswerLine . lines $ answers
  return items

-- | Parses one line of the answer file.
-- Each line names an input file, and has the expected answers
-- for part1 and part2 of the problem.  If the expected answers
-- aren't known yet, "?" takes the place of the answer.
parseAnswerLine :: (Read b, Read c) => String -> (String, (Maybe b, Maybe c))
parseAnswerLine line =
  (fileName, (readMaybe b, readMaybe c))
  where
    [fileName, b, c] = words line

readMaybe :: (Read x) => String -> Maybe x
readMaybe "?" = Nothing
readMaybe s = Just (read s)

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

-- | A point on a plane of Ints
type Point = V2 Int

-- | All of the neighbors of a point, but not the point itself
neighbors :: Point -> [Point]
neighbors p = filter (/= p) . neighborsAndSelf $ p

-- | All the neighbors of a point AND the point itself.
neighborsAndSelf :: Point -> [Point]
neighborsAndSelf p =
  [ p + V2 dx dy
    | dx <- [(-1) .. 1],
      dy <- [(-1) .. 1]
  ]

-- | A rectangle on a plane of Ints.
--
-- The bounds of the rectangle are inclusive.
data Rectangle
  = Rectangle Point Point
  deriving (Eq, Ord, Show)

-- | Does the rectangle contain the given point?
rectangleContains :: Rectangle -> Point -> Bool
rectangleContains (Rectangle (V2 x0 y0) (V2 x1 y1)) (V2 x y) =
  x0 <= x && x <= x1 && y0 <= y && y <= y1

-- | All of the points within a rectangle
rectanglePoints :: Rectangle -> [Point]
rectanglePoints (Rectangle (V2 x0 y0) (V2 x1 y1)) =
  [ V2 x y
    | x <- [x0 .. x1],
      y <- [y0 .. y1]
  ]

-- | A grid of characters.
--
-- Advent of Code problems frequently start as grids of characters.
-- The problems usualy index them 1-based, with (1, 1) at the upper
-- left, positive x going right, and positive y going down.
--
-- A grid is stored as a map from position to non-space character at
-- that position.
newtype Grid = Grid (M.Map (V2 Int) Char) deriving (Eq, Ord, Read, Show)

-- | An empty grid, with no cells filled.
gridEmpty :: Grid
gridEmpty = Grid M.empty

-- | Returns the character at a given position.
gridGet :: V2 Int -> Grid -> Char
gridGet p (Grid m) = M.findWithDefault ' ' p m

-- | Updates a grid to set the character at a given position
gridSet :: V2 Int -> Char -> Grid -> Grid
gridSet p c (Grid m) =
  if c == ' '
    then Grid (M.delete p m)
    else Grid (M.insert p c m)

-- | Extracts the map from a Grid
gridMap :: Grid -> M.Map (V2 Int) Char
gridMap (Grid m) = m

-- | Returns the contents of a grid as a list
gridToList :: Grid -> [(Point, Char)]
gridToList (Grid m) = M.toList m

-- | Converts a list to a Grid
gridFromList :: [(Point, Char)] -> Grid
gridFromList = Grid . M.fromList

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

-- | A grid that supports generalized Conway's Life
class LifeGrid g where
  -- | Create a new grid of the same type, with the given cells.
  -- The old grid is used as context, because some grid types
  -- have properties (such as size) that carry over to the new
  -- grid.
  lgNew :: g -> [(Point, Char)] -> g

  -- | Get the state of one point on the grid
  -- The result is ' ' if the point is on the grid, but
  -- there is nothing there.  The result is Nothing if
  -- the point is not valid for this grid.
  lgGet :: Point -> g -> Maybe Char

  -- | Returns all of the cells that have something present.
  lgCells :: g -> [(Point, Char)]

  -- | Returns all of the places that might have something
  -- in the next iteration.
  lgCandidates :: g -> [Point]

-- | Computes the next step in a conway-life-style simulation.
lifeStep :: (LifeGrid g) => (g -> Point -> [Point]) -> (Char -> [Char] -> Char) -> g -> g
lifeStep neighbors newCellState g =
  lgNew g result
  where
    result =
      [ (p, c)
        | p <- lgCandidates g,
          let c = newCellState (fromJust $ get p) (mapMaybe get (neighbors g p)),
          c /= ' '
      ]
    get p = lgGet p g

-- | Sequence of states of a conway-life-style simulation
life :: (LifeGrid g) => (g -> Point -> [Point]) -> (Char -> [Char] -> Char) -> g -> [g]
life neighbors newCellState = iterate (lifeStep neighbors newCellState)

-- | In a list, replace all occurrances of `a` with `b`
--
-- >>> replace 2 9 [1, 2, 3]
-- [1,9,3]
replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)
