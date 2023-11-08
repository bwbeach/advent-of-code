module Main where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

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

fib :: Int -> Integer
fib =
  runMemoize fib'
  where
    fib' =
      memoize
        ( \n ->
            if n < 2
              then return 1
              else do
                a <- fib' (n - 1)
                b <- fib' (n - 2)
                return (a + b)
        )

main :: IO ()
main =
  do
    print (fib 100)