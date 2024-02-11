module Main where

import Advent
  ( memoize,
    run,
    runMemoize,
  )
import Data.Bits
  ( Bits (complement, shiftL, shiftR, (.&.), (.|.)),
  )
import Data.Char (isNumber)
import Data.List.Extra (dropEnd)
import qualified Data.Map.Strict as M
import Data.Word (Word16)

main :: IO ()
main = run parse part1 part2

data Monop
  = NOT
  deriving (Eq, Ord, Read, Show)

data Binop
  = AND
  | OR
  | LSHIFT
  | RSHIFT
  deriving (Eq, Ord, Read, Show)

data Definition
  = Const Word16
  | Name String
  | Op1 Monop Definition
  | Op2 Binop Definition Definition
  deriving (Eq, Ord, Show)

type Problem = M.Map String Definition

parse :: String -> Problem
parse = M.fromList . map parseLine . lines

parseLine :: String -> (String, Definition)
parseLine =
  go . words
  where
    go ws = (last ws, parseLhs (dropEnd 2 ws))

    parseLhs [a] = parseAtom a
    parseLhs [op, a] = Op1 (read op) (parseAtom a)
    parseLhs [a, op, b] = Op2 (read op) (parseAtom a) (parseAtom b)
    parseLhs x = error ("bad lhs: " ++ show x)

    parseAtom a =
      if isNumber (head a)
        then Const (read a)
        else Name a

part1 :: Problem -> Word16
part1 problem = eval problem (Name "a")

part2 :: Problem -> Word16
part2 problem =
  eval problem2 (Name "a")
  where
    problem2 = M.insert "b" (Const (part1 problem)) problem

-- | Evaluate a definition
-- These test cases come from the problem description
--
-- >>> eval M.empty (Op2 AND (Const 123) (Const 456))
-- 72
--
-- >>> eval M.empty (Op2 OR (Const 123) (Const 456))
-- 507
--
-- >>> eval M.empty (Op2 LSHIFT (Const 123) (Const 2))
-- 492
--
-- >>> eval M.empty (Op2 RSHIFT (Const 456) (Const 2))
-- 114
--
-- >>> eval M.empty (Op1 NOT (Const 456))
-- 65079
eval :: Problem -> Definition -> Word16
eval problem =
  runMemoize go
  where
    go = memoize go'

    go' defn =
      case defn of
        Const c -> return c
        Name s -> case M.lookup s problem of
          Nothing -> error ("undefined name: " ++ s)
          Just d -> go d
        Op1 op a -> do
          va <- go a
          return (op1Fcn op va)
        Op2 op a b -> do
          va <- go a
          vb <- go b
          return (op2Fcn op va vb)

op1Fcn :: Monop -> Word16 -> Word16
op1Fcn NOT = complement

op2Fcn :: Binop -> Word16 -> Word16 -> Word16
op2Fcn AND = (.&.)
op2Fcn OR = (.|.)
op2Fcn LSHIFT = \a b -> shiftL a (fromEnum b)
op2Fcn RSHIFT = \a b -> shiftR a (fromEnum b)
