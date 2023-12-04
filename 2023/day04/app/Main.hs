{-# LANGUAGE TupleSections #-}

module Main where

import Advent (run)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)

import Debug.Trace

main :: IO ()
main = run parse part1 part2

type Card = (Int, S.Set Int, S.Set Int)

type Problem = [Card]

-- 
parse :: String -> Problem
parse = map parseLine . lines

-- | Parses one line that looks like this:
-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
parseLine :: String -> Card 
parseLine s =
    (read . drop 5 $ a, winners, have)
    where 
        [a, b] = splitOn ": " s
        [c, d] = splitOn " | " b 
        winners = S.fromList . map read . words $ c 
        have = S.fromList . map read . words $ d

part1 :: Problem -> Int
part1 = 
    sum . map cardScore 
    where 
         cardScore (_, w, h) = exp2 . length . filter (`S.member` w) . S.toList $ h 

         exp2 0 = 0
         exp2 1 = 1
         exp2 n = 2 * exp2 (n - 1)

part2 :: Problem -> Int
part2 cards = 
    sum . M.elems . foldl' runCard initial $ [1 .. length cards]
    where 
        initial = M.fromList . map (, 1) $ [1 .. length cards]
        runCard s n = foldl' (bumpCard (s M.! n)) s [(n + 1) .. (n + cardScore n)]
        bumpCard i s n = M.insert n ((s M.! n) + i) s
        cardScore n = 
            length . filter (`S.member` w) . S.toList $ h 
            where 
                (w, h) = cardMap M.! n
        cardMap = M.fromList . map (\(n, w, h) -> (n, (w, h))) $ cards

