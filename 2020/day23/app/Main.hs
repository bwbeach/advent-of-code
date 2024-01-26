module Main where

import Advent (run)
import Data.Char

main :: IO ()
main = run parse part1 part2

-- | A ring of cups
-- The first one in the list is the current cup, followed by the 
-- cups to the right, going around the ring.  The last one in the
-- list is the cup to the left of the current cup.
newtype Ring = 
    Ring [Int] 
    
-- | Show-ing a ring displays the form from the input, like "389125467"
instance Show Ring where
    show (Ring items) = concatMap show items 

-- | Parsing a ring is the inverse of showing it 
--
-- >>> parseRing "12345"
-- 12345
parseRing :: String -> Ring 
parseRing s = Ring (map (\c -> read [c]) s)

-- | What is the current cup?
currentCup :: Ring -> Int 
currentCup (Ring (a : _)) = a
currentCup r = error ("Ring is empty: " ++ show r)

-- | Take the three cups to the right of the current cup. 
-- Returns: (removedCups, updatedRing)
--
-- >>> takeThree (parseRing "123456")
-- ([2,3,4],156)
takeThree :: Ring -> ([Int], Ring)
takeThree (Ring (a : b : c : d : es)) = ([b, c, d], Ring (a : es))
takeThree r = error ("Ring does not have enough cups to take three: " ++ show r)

-- | Pick the destination cup, given the current cup number, and cups that should be excluded. 
--
-- >>> destinationCup 5 [4, 2]
-- 3
--
-- >>> destinationCup 2 [1, 9]
-- 8
destinationCup :: Int -> [Int] -> Int 
destinationCup c exclude = head . filter (not . (`elem` exclude)) $ [(c - 1), (c - 2) .. 1] ++ [9, 8 .. (c + 1)]

-- | Add cups after the given one
--
-- >>> addAfter [2, 4] 5 (parseRing "1357")
-- 135247
addAfter :: [Int] -> Int -> Ring -> Ring 
addAfter toAdd dest (Ring cs) =
    Ring (go cs)
    where 
        go [] = error "Destination cup not found"
        go (x : xs)
          | x == dest = x : (toAdd ++ xs)
          | otherwise = x : go xs

-- | Update the ring so the current cup is one to the right 
--
-- >>> moveCurrentRight (parseRing "12345")
-- 23451
moveCurrentRight :: Ring -> Ring 
moveCurrentRight (Ring (x : xs)) = Ring (xs ++ [x])
moveCurrentRight r = error ("Ring has no current cup: " ++ show r)

-- | What's the order of the cups after cup 1?
--
-- >>> orderAfterOne (parseRing "456123")
-- 23456
orderAfterOne :: Ring -> Ring 
orderAfterOne (Ring (1 : xs)) = Ring xs 
orderAfterOne r = orderAfterOne . moveCurrentRight $ r

-- | One round of the game 
--
-- >>> oneRound (parseRing "546789132")
-- 891346725
oneRound :: Ring -> Ring 
oneRound r =
    r'''
    where 
        -- pick up three cups
        (removed, r') = takeThree r
        -- select the destination 
        dest = destinationCup (currentCup r') removed 
        -- put the removed cups back to the right of the destination
        r'' = addAfter removed dest r' 
        -- make the next cup the current one 
        r''' = moveCurrentRight r''


-- | The input file contains a Ring
type Problem = Ring

parse :: String -> Problem
parse = parseRing . filter isDigit

part1 :: Problem -> String
part1 = show . orderAfterOne . (!! 100) . iterate oneRound

part2 :: Problem -> Int
part2 _ = 0
