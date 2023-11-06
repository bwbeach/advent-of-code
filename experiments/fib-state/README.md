I started with a working example of a state monad from the 
[Haskell Wiki](https://wiki.haskell.org/State_Monad).  It stores
an Int in the state.

My first change is to add a state and a monad to hold it:

```haskell
type MemoState = M.Map Int Integer
type MemoMonad = State MemoState
```

Then a monadic version of `fib` that doesn't actually use the state yet:

```haskell
fib :: Int -> MemoMonad Integer
fib n = do
  if n < 2
    then return 1
    else do
      a <- fib (n - 1)
      b <- fib (n - 2)
      return (a + b)
```

Now, let's try putting the results in the state as they are computed.  Still not
using them.

```haskell
-- | Store a result in the memo map.
insert :: Int -> Integer -> MemoMonad ()
insert k v =
    state (\ms -> ((), M.insert k v ms))

-- | Compute Fibonacci number
fib :: Int -> MemoMonad Integer
fib n = do
  if n < 2
    then return 1
    else do
      a <- fib (n - 1)
      b <- fib (n - 2)
      let result = a + b
      insert n result
      return result
```

Changing `main` to use `runState`, which returns both the answer and the new state, 
we can see that it actually did insert the values.  Yay!

```
(8,fromList [(2,2),(3,3),(4,5),(5,8)])
```

Now, let's see about using those values.

153946181
