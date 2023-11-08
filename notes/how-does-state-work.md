# How does the State monad work?

It's taking me a long time to get my head around monads and passing state.
One thing that frequently helps me to understand things is to go through
what happens under the covers.

## A simple program 

Let's take a very simple program that uses `State`:

```haskell
type SimpleState = Int

type SimpleMonad = State SimpleState

postIncrement :: SimpleMonad Int
postIncrement = state (\state -> (state, state + 1))

simple :: Int -> SimpleMonad Int
simple n = do
  x <- postIncrement
  return (x * n)

go :: (Int, Int)
go = runState (simple 5) 2

mymain :: IO ()
mymain = print go
```

The high-level view of what happens here is that the state starts at `2`,
the `simple` function sets x to the current state while incrementing the state
to `3`, then returns `5 * 2`.  `runState` returns a pair containing the returned
value and the updated state:

```haskell
(10, 3)
```

## Definitions

Before we can do that, let's undo the syntactic sugar for `simple`:

```haskell
simple n = postIncrement >>= (\x -> return (x * n))
```

And some definitions we'll use:

```haskell

type State s = StateT s Identity
type SimpleMonad = StateT Int Indentity

```

## Step by step

How does it do that?  Let's go step by step, starting with `go` and replacing
things with their definitions one at a time.

```haskell
go
runState (simple 5) 2
runState (postIncrement >>= (\n -> return (x * n)))
```



