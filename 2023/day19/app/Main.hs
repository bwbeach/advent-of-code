module Main where

import Advent (run)
import Data.List.Extra (dropEnd, find)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import RangeMap
  ( Range (..),
    RangeMap,
    rangeSize,
    rmEmpty,
    rmMerge,
    rmSingleton,
    rmToList,
  )

main :: IO ()
main = run parse part1 part2

part1 :: Problem -> Int
part1 (workflows, parts) =
  sum . map scorePart . filter partAccepted $ parts
  where
    partAccepted p = runPart workflows p == "A"
    scorePart = sum . M.elems

part2 :: Problem -> Int
part2 = volume . allAcceptable . fst

type Part = M.Map Char Int

data Rule = Rule
  { field :: Char, -- which field of the part to check
    op :: Char, -- comparison operator, such as '<'
    val :: Int, -- value to compare to
    target :: String -- next workflow if there is a match
  }
  deriving (Show)

data Workflow = Workflow
  { rules :: [Rule],
    other :: String -- default target if no rule matches
  }
  deriving (Show)

type Problem = (M.Map String Workflow, [Part])

-- | Parse an entire Problem.
parse :: String -> Problem
parse text =
  (M.fromList . map parseWorkflow . lines $ workflowText, map parsePart (lines partText))
  where
    [workflowText, partText] = splitOn "\n\n" text
    parsePart = M.fromList . map parseField . splitOn "," . drop 1 . dropEnd 1
    parseField s = (head s, read . drop 2 $ s)

-- | Parse one workflow line.
--
-- >>> parseWorkflow "px{a<2006:qkq,m>2090:A,rfg}"
-- ("px",Workflow {rules = [Rule {field = 'a', op = '<', val = 2006, target = "qkq"},Rule {field = 'm', op = '>', val = 2090, target = "A"}], other = "rfg"})
parseWorkflow :: String -> (String, Workflow)
parseWorkflow s =
  (name, workflow)
  where
    [name, workflowText] = splitOn "{" (dropEnd 1 s)
    items = splitOn "," workflowText
    workflow =
      Workflow
        { rules = map parseRule . dropEnd 1 $ items,
          other = last items
        }
    parseRule s =
      Rule {field = f, op = o, val = read v, target = t}
      where
        [f : o : v, t] = splitOn ":" s

-- | Run one part through the workflows
runPart :: M.Map String Workflow -> Part -> String
runPart workflows part =
  go "in"
  where
    go workflowName =
      case runWorkflow (workflows M.! workflowName) part of
        "A" -> "A"
        "R" -> "R"
        wName -> go wName

-- | Run a part through one workflow, resulting in "A", "R", or another workflow name.
runWorkflow :: Workflow -> Part -> String
runWorkflow w p =
  maybe (other w) target . find (ruleMatches p) $ rules w

ruleMatches :: Part -> Rule -> Bool
ruleMatches p Rule {field = f, op = o, val = v} =
  checkOp (p M.! f) o v
  where
    checkOp a '<' b = a < b
    checkOp a '>' b = a > b
    checkOp a o b = error ("bad checkop " ++ show (a, o, b))

-- | The full range of possible values on one of the n axes.
fullSpan :: Range Int
fullSpan = Range 1 4000

-- | An n-dimensional space, listing what's acceptable
data RangeTree
  = RTLeaf
  | RTNode (RangeMap Int RangeTree)
  deriving (Eq, Show)

-- | An n-dimensional solid where all values are acceptable.
solidCube :: Int -> RangeTree
solidCube n = block (replicate n fullSpan)

-- | An n-dimensional space where no values are acceptable.
empty :: RangeTree
empty = RTNode rmEmpty

-- | Union of two shapes
--
-- >>> union (block [Range 1 4])  (block [Range 3 5])
-- RTNode rmFromList [(Range 1 5,RTLeaf)]
--
-- >>> union (block [Range 1 4, Range 1 4])  (block [Range 3 5, Range 3 5])
-- RTNode rmFromList [(Range 1 2,RTNode rmFromList [(Range 1 4,RTLeaf)]),(Range 3 4,RTNode rmFromList [(Range 1 5,RTLeaf)]),(Range 5 5,RTNode rmFromList [(Range 3 5,RTLeaf)])]
union :: RangeTree -> RangeTree -> RangeTree
union RTLeaf RTLeaf = RTLeaf
union (RTNode ma) (RTNode mb) =
  RTNode (rmMerge go ma mb)
  where
    go Nothing Nothing = Nothing
    go (Just a) Nothing = Just a
    go Nothing (Just a) = Just a
    go (Just a) (Just b) = Just (a `union` b)

-- | Intersection of two shapes
--
-- >>> intersection (block [Range 1 4])  (block [Range 3 5])
-- RTNode rmFromList [(Range 3 4,RTLeaf)]
--
-- >>> intersection (block [Range 1 4, Range 1 4])  (block [Range 3 5, Range 3 5])
-- RTNode rmFromList [(Range 3 4,RTNode rmFromList [(Range 3 4,RTLeaf)])]
intersection :: RangeTree -> RangeTree -> RangeTree
intersection RTLeaf RTLeaf = RTLeaf
intersection (RTNode ma) (RTNode mb) =
  RTNode (rmMerge go ma mb)
  where
    go Nothing _ = Nothing
    go _ Nothing = Nothing
    go (Just a) (Just b) = Just (a `intersection` b)

-- | A rectanguler block, with given dimensions along each axis
block :: [Range Int] -> RangeTree
block =
  foldr mkTree RTLeaf
  where
    mkTree range child = RTNode (rmSingleton range child)

-- | The volume of a shape
--
-- >>> volume $ union (block [Range 1 4])  (block [Range 3 5])
-- 5
--
-- >>> volume $ union (block [Range 1 4, Range 1 4])  (block [Range 3 5, Range 3 5])
-- 21
volume :: RangeTree -> Int
volume RTLeaf = 1
volume (RTNode rm) =
  sum . map rangeVolume . rmToList $ rm
  where
    rangeVolume (r, rm') = rangeSize r * volume rm'

-- | Make the shape that is all acceptable parts
allAcceptable :: M.Map String Workflow -> RangeTree
allAcceptable workflows =
  goWorkflow "in"
  where
    goWorkflow "A" = solidCube 4
    goWorkflow "R" = empty
    goWorkflow name =
      foldr addRule (goWorkflow (other w)) (rules w)
      where
        w = workflows M.! name

    addRule Rule {field = f, op = o, val = v, target = t} restOfWorkflow =
      ifTrue `union` ifFalse
      where
        ifTrue = ruleCondition f o v `intersection` goWorkflow t
        ifFalse = inverseCondition f o v `intersection` restOfWorkflow

-- | The shape that makes a rule condition true
--
-- >>> ruleCondition 'm' '<' 1000
-- RTNode rmFromList [(Range 1 4000,RTNode rmFromList [(Range 1 999,RTNode rmFromList [(Range 1 4000,RTNode rmFromList [(Range 1 4000,RTLeaf)])])])]
ruleCondition :: Char -> Char -> Int -> RangeTree
ruleCondition f o v =
  forField f (cond o v)
  where
    forField 'x' r = block [r, fullSpan, fullSpan, fullSpan]
    forField 'm' r = block [fullSpan, r, fullSpan, fullSpan]
    forField 'a' r = block [fullSpan, fullSpan, r, fullSpan]
    forField 's' r = block [fullSpan, fullSpan, fullSpan, r]

    cond '<' v = Range 1 (v - 1)
    cond '>' v = Range (v + 1) 4000

-- | The shape that makes a rule condition false
--
-- >>> inverseCondition 'm' '<' 1000
-- RTNode rmFromList [(Range 1 4000,RTNode rmFromList [(Range 1000 4000,RTNode rmFromList [(Range 1 4000,RTNode rmFromList [(Range 1 4000,RTLeaf)])])])]
inverseCondition :: Char -> Char -> Int -> RangeTree
inverseCondition f '<' v = ruleCondition f '>' (v - 1)
inverseCondition f '>' v = ruleCondition f '<' (v + 1)
