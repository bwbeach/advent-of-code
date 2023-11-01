# advent-of-code

## Overview

Collection of all of my Advent of Code attempts and solutions in Haskell.

## Useful libraries

I've found it difficult, while learning Haskell, to find the libraries to use
for a variety of tasks.  When I search for something like `splitOn` in Hoogle,
there's a bunch of results, and it takes some searching to find the right one.

This list names the package, the library it's in, and a list of the most useful functions.

### [Data.List](base)

A bunch of handy things for lists.

 - `groupBy` - Groups elements of a list

### [Data.Map.Strict](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Strict.html) (containers)

A key-to-value map.

 - `fromList'
 - `lookup`
