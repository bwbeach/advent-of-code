module Main where

import Advent (run)
import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

main :: IO ()
main = run parse part1 part2

-- | A ring of things, with a designated current thing.
-- Representation is a map from item to the thing to its right.
data Ring a = Ring
  { curr :: a,
    next :: M.Map a a
  }
  deriving (Show)

-- | Create a Ring from a list of things
-- The back of the list wraps back to the front
--
-- >>> makeRing [1, 2, 3]
-- Ring {curr = 1, next = fromList [(1,2),(2,3),(3,1)]}
makeRing :: (Ord a) => [a] -> Ring a
makeRing (a : as) =
  Ring {curr = a, next = go a as M.empty}
  where
    go b (c : ds) m = go c ds (M.insert b c m)
    go b [] m = M.insert b a m
makeRing _ = error "bug"

-- | Return the cup to the right of the given one 
getRight :: Ord a => a -> Ring a -> a 
getRight a r = next r M.! a

-- | Remove the item to the right of the current one
-- Returns (itemRemoved, newRing)
--
-- >>> removeRight (makeRing [1, 2, 3])
removeRight :: (Ord a) => Ring a -> (a, Ring a)
removeRight r@(Ring {curr = a, next = m}) =
  (b, r {next = m'})
  where
    b = m M.! a
    c = m M.! b
    m' = M.insert a c . M.delete b $ m

-- | Adds an element after element a
--
-- >>> addAfter 2 3 (makeRing [1, 2, 4])
-- Ring {curr = 1, next = fromList [(1,2),(2,3),(3,4),(4,1)]}
addAfter :: (Ord a) => a -> a -> Ring a -> Ring a
addAfter a b r@(Ring {next = m}) =
  r {next = m'}
  where
    c = m M.! a
    m' = M.insert a b . M.insert b c $ m

-- | Take the three cups to the right of the current cup.
-- Returns: (removedCups, updatedRing)
--
-- >>> takeThree (makeRing [(1 :: Int) .. 6])
-- ([2,3,4],Ring {curr = 1, next = fromList [(1,5),(5,6),(6,1)]})
takeThree :: (Ord a) => Ring a -> ([a], Ring a)
takeThree r =
  ([a, b, c], r''')
  where
    (a, r') = removeRight r
    (b, r'') = removeRight r'
    (c, r''') = removeRight r''

-- | Pick the destination cup, given the current cup number, and cups that should be excluded.
--
-- >>> destinationCup (makeRing [5, 6, 2, 3])
-- 3
--
-- >>> destinationCup (makeRing [5, 6, 7, 8])
-- 8
destinationCup :: Ring Int -> Int
destinationCup (Ring { curr = a, next = m }) =
  fst $ fromMaybe (M.findMax m) (M.lookupLT a m)

-- | Update the ring so the current cup is one to the right
--
-- >>> moveCurrentRight (parseRing "12345")
-- 23451
moveCurrentRight :: Ord a => Ring a -> Ring a
moveCurrentRight r@(Ring {curr = a, next = m}) = r { curr = m M.! a } 

-- | What's the order of the cups after cup 1?
--
-- >>> orderAfter 1 (makeRing [4, 5, 6, 1, 2 ,3])
-- [2,3,4,5,6]
orderAfter :: Ord a => a -> Ring a -> [a]
orderAfter start (Ring {next = m})= 
    go start 
    where 
        go a = 
            let b = m M.! a in
                if b == start 
                    then [] 
                    else b : go b

-- | One round of the game
--
-- >>> oneRound (parse "546789132")
-- Ring {curr = 8, next = fromList [(1,3),(2,5),(3,4),(4,6),(5,8),(6,7),(7,2),(8,9),(9,1)]}
oneRound :: Ring Int -> Ring Int
oneRound r0 = 
    r7
    where
      -- pick up three cups
      (a, r1) = removeRight r0 
      (b, r2) = removeRight r1 
      (c, r3) = removeRight r2
      -- select the destination
      dest = destinationCup r3
      -- put the removed cups back to the right of the destination
      r4 = addAfter dest c r3
      r5 = addAfter dest b r4
      r6 = addAfter dest a r5
      -- make the next cup the current one
      r7 = moveCurrentRight r6

-- | The input file contains a Ring
type Problem = [Int]

parse :: String -> Problem
parse = map (\c -> read [c]) . filter isDigit

part1 :: Problem -> String
part1 = concatMap show . orderAfter 1 . (!! 100) . iterate oneRound . makeRing

part2 :: Problem -> Int
part2 problem = 
  a * b
  where 
    start = makeRing $ problem ++ [maximum problem + 1 .. 1000000]
    finish = (!! 10000000) . iterate oneRound $ start
    a = getRight 1 finish
    b = getRight a finish

