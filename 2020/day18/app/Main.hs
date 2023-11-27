{-# LANGUAGE MultiWayIf #-}

module Main where

import Advent (run)
import Control.Monad.State.Lazy
  ( MonadState (state),
    State, 
    evalState,
  )
import Data.Char (isDigit)
import Data.List (group)
import GHC.Base (bindIO)

main :: IO ()
main = run parse part1 part2

type Problem = [String]

parse :: String -> Problem
parse = lines . filter (/= ' ')

part1 :: Problem -> Integer
part1 = sum . map eval

type EvalState v = State String v

peekChar :: EvalState (Maybe Char)
peekChar = state go 
  where 
    go [] = (Nothing, [])
    go (c : cs) = (Just c, c : cs)

readChar :: EvalState Char 
readChar = state (\(c : cs) -> (c, cs))

expectChar :: Char -> EvalState ()
expectChar c = do
    c' <- readChar 
    if c == c' 
        then return ()
        else error ("expected: " ++ [c])

doOp :: Num a => Char -> a -> a -> a
doOp '+' a b = a + b
doOp '*' a b = a * b 

isJustOp :: Maybe Char -> Bool
isJustOp (Just c) = isOp c 
isJustOp Nothing = False 

isOp :: Char -> Bool
isOp c = c `elem` "+*"

eval :: String -> Integer 
eval =
    evalState primaryAndBinops
    where
        primaryAndBinops = do
            n <- primary
            binops n
        binops n = do
            c <- peekChar
            if isJustOp c 
                then do
                    c' <- readChar
                    r <- primary
                    binops (doOp c' n r)
                else return n
        primary = do
            c <- readChar
            if
                | c == '(' -> do
                    n <- primaryAndBinops 
                    expectChar ')'
                    return n
                | isDigit c -> return (read [c])
                | otherwise -> error ("bad primary: " ++ [c])

part2 :: Problem -> Integer
part2 = sum . map eval2

eval2 :: String -> Integer 
eval2 =
    evalState stuffAndStars
    where
        stuffAndStars = do
            n <- stuffAndPlusses
            stars n
        stars n = do
            c <- peekChar
            if c == Just '*' 
                then do
                    c' <- readChar
                    r <- stuffAndPlusses
                    stars (n * r)
                else return n
        stuffAndPlusses = do
            n <- primary
            plusses n
        plusses n = do
            c <- peekChar
            if c == Just '+' 
                then do
                    c' <- readChar
                    r <- primary
                    plusses (n + r)
                else return n
        primary = do
            c <- readChar
            if
                | c == '(' -> do
                    n <- stuffAndStars 
                    expectChar ')'
                    return n
                | isDigit c -> return (read [c])
                | otherwise -> error ("bad primary: " ++ [c])
