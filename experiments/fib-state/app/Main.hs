module Main where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

-- | The memoization state for a Fibonacci calculation.
newtype Memo k v = Memo (M.Map k v)

-- | The monad that has a MemoState and an Integer result
type MemoMonad = State (Memo Int Integer)

memoLookup :: Int -> MemoMonad (Maybe Integer)
memoLookup k =
  state lookup
  where
    lookup (Memo ms) =
      case M.lookup k ms of
        Nothing -> (Nothing, Memo ms)
        Just v' -> (Just v', Memo ms)

-- | Store a result in the memo map.
memoInsert :: Int -> Integer -> MemoMonad ()
memoInsert k v =
  state (\(Memo ms) -> ((), Memo $ M.insert k v ms))

memoize :: (Int -> MemoMonad Integer) -> Int -> MemoMonad Integer
memoize f n = do
  maybeResult <- memoLookup n
  case maybeResult of
    Nothing -> do
      result <- f n
      memoInsert n result
      return result
    Just result -> do
      return result

runMemoize :: (Int -> MemoMonad Integer) -> Int -> Integer
runMemoize m x =
  evalState (m x) (Memo M.empty)

fib2 :: Int -> MemoMonad Integer
fib2 =
  memoize fib2'
  where
    fib2' n =
      if n < 2
        then return 1
        else do
          a <- fib2 (n - 1)
          b <- fib2 (n - 2)
          return (a + b)

main :: IO ()
main =
  do
    print (runMemoize fib2 100)