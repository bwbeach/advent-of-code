module Main where

import Advent
  ( Point,
    run,
  )
import Data.List (foldl')
import Data.List.Split (splitOn)
import Debug.Trace
import Linear.V2 (V2 (..))
import RangeMap

main :: IO ()
main = run parse part1 part2

data Rect
  = Rect Point Point
  deriving (Show)

data Action
  = TURN_ON
  | TURN_OFF
  | TOGGLE
  deriving (Show)

type Instruction = (Action, Rect)

type Problem = [Instruction]

parse :: String -> Problem
parse = map parseLine . lines

-- | Parse one input line
--
-- >>> parseLine "turn on 158,270 through 243,802"
-- (TURN_ON,Rect (V2 158 270) (V2 243 802))
parseLine :: String -> Instruction
parseLine =
  go . words
  where
    go ("turn" : "on" : rect) = (TURN_ON, parseRect rect)
    go ("turn" : "off" : rect) = (TURN_OFF, parseRect rect)
    go ("toggle" : rect) = (TOGGLE, parseRect rect)
    go x = error ("bad line: " ++ show x)

    parseRect [a, "through", b] = Rect (parsePoint a) (parsePoint b)
    parseRect x = error ("bad rect: " ++ show x)

    parsePoint =
      goPoint . map read . splitOn ","
      where
        goPoint [x, y] = V2 x y
        goPoint x = error ("bad point: " ++ show x)

part1 :: Problem -> Int
part1 problem = totalBrightness $ applyInstructions problem initial

part2 :: Problem -> Int
part2 problem = totalBrightness $ applyInstructions2 problem initial

type Column = RangeMap Int Int

type Grid = RangeMap Int Column

initial :: Grid
initial = rmSingleton (Range 0 999) (rmSingleton (Range 0 999) 0)

applyInstructions :: Problem -> Grid -> Grid
applyInstructions problem before =
  foldl' oneStep before problem
  where
    oneStep g i = applyInstruction i g

applyInstruction :: Instruction -> Grid -> Grid
applyInstruction (action, Rect (V2 x0 y0) (V2 x1 y1)) =
  rmUpdate (Range x0 x1) updateColumn
  where
    updateColumn :: Maybe Column -> Maybe Column
    updateColumn (Just c) = Just (rmUpdate (Range y0 y1) updateCell c)
    updateColumn Nothing = error "did not expect column to be Nothing"

    updateCell = case action of
      TURN_ON -> turnOn
      TURN_OFF -> turnOff
      TOGGLE -> toggle

turnOn :: Maybe Int -> Maybe Int
turnOn = const (Just 1)

turnOff :: Maybe Int -> Maybe Int
turnOff = const (Just 0)

toggle :: Maybe Int -> Maybe Int
toggle (Just x) = Just (1 - x)
toggle Nothing = error "did not expect Nothing"

totalBrightness :: Grid -> Int
totalBrightness =
  sum . map brightnessInColRange . rmToList
  where
    brightnessInColRange (Range x0 x1, column) = (x1 - x0 + 1) * brightnessInColumn column

    brightnessInColumn = sum . map brightnessInRange . rmToList

    brightnessInRange (Range y0 y1, setting) = (y1 - y0 + 1) * setting

applyInstructions2 :: Problem -> Grid -> Grid
applyInstructions2 problem before =
  foldl' oneStep before problem
  where
    oneStep g i = applyInstruction2 i g

applyInstruction2 :: Instruction -> Grid -> Grid
applyInstruction2 (action, Rect (V2 x0 y0) (V2 x1 y1)) =
  rmUpdate (Range x0 x1) updateColumn
  where
    updateColumn :: Maybe Column -> Maybe Column
    updateColumn (Just c) = Just (rmUpdate (Range y0 y1) updateCell c)
    updateColumn Nothing = error "did not expect column to be Nothing"

    updateCell (Just n) = Just (max 0 (n + delta))
    updateCell Nothing = error "did not expect cell to be Nothing"

    delta = case action of
      TURN_ON -> 1
      TURN_OFF -> (-1)
      TOGGLE -> 2