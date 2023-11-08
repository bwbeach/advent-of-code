module Main where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

-- non monadic version of a very simple state example
-- the State is an integer.
-- the value will always be the negative of the state

type MyState = Int

valFromState :: MyState -> Int
valFromState s = -s

nextState :: MyState -> MyState
nextState x = 1 + x

-- | The memoization state for a Fibonacci calculation.
type MemoState = M.Map Int Integer

type MyStateMonad = State MyState

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

-- this is it, the State transformation.  Add 1 to the state, return -1*the state as the computed value.
getNext :: MyStateMonad Int
getNext = state (\st -> let st' = nextState (st) in (valFromState (st'), st'))

-- | Store a result in the memo map.
insert :: Int -> Integer -> MemoMonad ()
insert k v =
  state (\ms -> ((), M.insert k v ms))

-- advance the state three times.
inc3 :: MyStateMonad Int
inc3 =
  getNext >>= \x ->
    getNext >>= \y ->
      getNext >>= \z ->
        return z

-- advance the state three times with do sugar
inc3Sugared :: MyStateMonad Int
inc3Sugared = do
  x <- getNext
  y <- getNext
  z <- getNext
  return z

-- advance the state three times without inspecting computed values
inc3DiscardedValues :: MyStateMonad Int
inc3DiscardedValues = getNext >> getNext >> getNext

-- advance the state three times without inspecting computed values with do sugar
inc3DiscardedValuesSugared :: MyStateMonad Int
inc3DiscardedValuesSugared = do
  getNext
  getNext
  getNext

-- advance state 3 times, compute the square of the state
inc3AlternateResult :: MyStateMonad Int
inc3AlternateResult = do
  getNext
  getNext
  getNext
  s <- get
  return (s * s)

-- advance state 3 times, ignoring computed value, and then once more
inc4 :: MyStateMonad Int
inc4 = do
  inc3AlternateResult
  getNext

-- | Super simple example
superSimple :: Int -> MemoMonad Integer
superSimple n =
  insert n 5 >>= \a -> return 6

remember :: (Int -> MemoMonad Integer) -> Int -> MemoMonad Integer
remember f n = do
  result <- f n
  insert n result
  return result

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

-- | Compute Fibonacci number
fib :: Int -> MemoMonad Integer
fib n = do
  if n < 2
    then return 1
    else do
      maybeResult <- memoLookup n
      case maybeResult of
        Nothing -> do
          a <- fib (n - 1)
          b <- fib (n - 2)
          let result = a + b
          insert n result
          return result
        Just result -> do
          return result

type SimpleState = Int

type SimpleMonad = State SimpleState

-- postIncrement :: SimpleMonad Int
postIncrement = state (\state -> (state, state + 1))

-- simple :: Int -> SimpleMonad Int
simple n = do
  x <- postIncrement
  return (x * n)

simple2 :: Int -> SimpleMonad Int
simple2 n = postIncrement >>= (\x -> return (x * n))

go :: (Int, Int)
go = runState (simple2 5) 2

mymain :: IO ()
mymain = print go

main :: IO ()
main =
  do
    print (evalState inc3 0) -- -3
    print (evalState inc3Sugared 0) -- -3
    print (evalState inc3DiscardedValues 0) -- -3
    print (evalState inc3DiscardedValuesSugared 0) -- -3
    print (evalState inc3AlternateResult 0) -- 9
    print (evalState inc4 0) -- -4
    print (runState (superSimple 3) M.empty)
    print (runState (fib 100) M.empty)
    print (runState (fib2 100) M.empty)
    mymain