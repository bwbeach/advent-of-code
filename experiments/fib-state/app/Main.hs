module Main where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

-- | The memoization state for a Fibonacci calculation.
type MemoState = M.Map Int Integer

-- | The monad that has a MemoState and an Integer result
type MemoMonad = State MemoState

memoLookup :: Int -> MemoMonad (Maybe Integer)
memoLookup k =
  state lookup
  where
    lookup ms =
      case M.lookup k ms of
        Nothing -> (Nothing, ms)
        Just v' -> (Just v', ms)

-- | Store a result in the memo map.
insert :: Int -> Integer -> MemoMonad ()
insert k v =
  state (\ms -> ((), M.insert k v ms))

memoize :: (Int -> MemoMonad Integer) -> Int -> MemoMonad Integer
memoize f n = do
  maybeResult <- memoLookup n
  case maybeResult of
    Nothing -> do
      result <- f n
      insert n result
      return result
    Just result -> do
      return result

fib2 :: Int -> MemoMonad Integer
fib2 = do
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
    print (runState (fib2 100) M.empty)