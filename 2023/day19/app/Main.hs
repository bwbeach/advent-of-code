module Main where

import Advent (run)
import Data.List.Extra (dropEnd, find)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

main :: IO ()
main = run parse part1 part2

part1 :: Problem -> Int
part1 (workflows, parts) =
  sum . map scorePart . filter partAccepted $ parts
  where
    partAccepted p = runPart workflows p == "A"
    scorePart = sum . M.elems

part2 :: Problem -> Int
part2 = length . snd

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

runPart :: M.Map String Workflow -> Part -> String
runPart workflows part =
  go "in"
  where
    go workflowName =
      case runWorkflow (workflows M.! workflowName) part of
        "A" -> "A"
        "R" -> "R"
        wName -> go wName

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