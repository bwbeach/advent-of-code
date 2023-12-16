{-# LANGUAGE TupleSections #-}

module Main where

import Advent (run)
import Data.Char (ord)
import Data.List (foldl')
import Data.List.Extra (dropEnd)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main = run parse part1 part2

type Problem = [String]

parse :: String -> Problem
parse = splitOn "," . filter (/= '\n')

part1 :: Problem -> Int
part1 = sum . map hash

part2 :: Problem -> Int
part2 = score . runInstructions . map parseInstruction

-- | Compute the score of the final set of boxes
--
-- >>> score (M.fromList [(0, [("rn", 1), ("cm", 2)]), (3, [("ot", 7), ("ab", 5), ("pc", 6)])])
-- 145
score :: M.Map Int Box -> Int
score =
  sum . map scoreEntry . M.toList
  where
    scoreEntry :: (Int, Box) -> Int
    scoreEntry (boxNum, contents) =
      sum $ zipWith scoreLens [1 ..] contents
      where
        scoreLens i (_, fl) = (boxNum + 1) * i * fl

data Instruction
  = AddLens String Int
  | RemLens String
  deriving (Show)

-- | Splits one instruction into its three parts.
parseInstruction :: String -> Instruction
parseInstruction s =
  if last s == '-'
    then RemLens (dropEnd 1 s)
    else AddLens addLabel (read addFocalLength)
  where
    [addLabel, addFocalLength] = splitOn "=" s

-- | Return the label in an instruction
instructionLabel :: Instruction -> String
instructionLabel instr =
  case instr of
    AddLens label _ -> label
    RemLens label -> label

-- | Applies a list of instructions to a an empty set of boxes
runInstructions :: [Instruction] -> M.Map Int Box
runInstructions =
  foldl' runOne emptyBoxes
  where
    emptyBoxes = M.fromList (map (,[]) [0 .. 255])

    runOne boxes instruction =
      M.insert boxNum updatedBox boxes
      where
        boxNum = hash (instructionLabel instruction)
        originalBox = boxes M.! boxNum
        updatedBox = action originalBox
        action =
          case instruction of
            RemLens label -> removeFromBox label
            AddLens label focalLength -> addToBox label focalLength

-- | The contents of a box, a list of (label, focalLength)
type Box = [(String, Int)]

-- | Follow a "-" instruction to remove a lens with the given label from a box.
--
-- >>> removeFromBox "pc" [("pc", 4), ("ot", 9), ("ab", 5)]
-- [("ot",9),("ab",5)]
removeFromBox :: String -> Box -> Box
removeFromBox label = filter ((/= label) . fst)

-- | Add a new lens and focal length, replacing any present, or adding at end.
--
-- >>> addToBox "cm" 2 [("rn" ,1)]
-- [("rn",1),("cm",2)]
--
-- >>> addToBox "rn" 5 [("rn",1),("cm",2)]
-- [("rn",5),("cm",2)]
addToBox :: String -> Int -> Box -> Box
addToBox label focalLength box =
  if any ((== label) . fst) box
    then map (updateLens label focalLength) box
    else box ++ [(label, focalLength)]
  where
    updateLens l f (l', f') =
      if l == l'
        then (l, f)
        else (l', f')

hash :: String -> Int
hash =
  foldl' hashChar 0
  where
    hashChar h c = ((h + ord c) * 17) `mod` 256
