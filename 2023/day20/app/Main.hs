{-# LANGUAGE TupleSections #-}

module Main where

import Advent (run)
import Data.Function ((&))
import Data.List (findIndex, foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Tuple (swap)
import Data.Tuple.Extra (second)
import Debug.Trace

main :: IO ()
main = run parse part1 part2

part1 :: Modules -> Int
part1 modules =
  modules
    -- Create the initial state
    & initialState
    -- Push the button as many times as needed
    & iterate (pushButton modules)
    -- Take the result of the 1000-th push
    & (!! 1000)
    -- Extract the counts
    & machineCounts
    -- Multiple the low count and high count
    & (\mc -> mcLow mc * mcHigh mc)

part2 :: Modules -> Int
part2 modules =
  if hasRx
    then rxIndex
    else -1
  where
    rxIndex =
      modules
        -- Create the initial state
        & initialState
        -- Push the button as many times as needed
        & iterate (pushButton modules)
        -- Extract the counts
        & map machineCounts
        -- How many times until rx count is non-zero?
        & findIndex (\mc -> mcLowToRx mc > 0)
        -- Extract answer
        & fromJust
    -- Does any module have an ouput names "rx"?
    hasRx =
      any (`moduleHasOutput` "rx") (M.elems modules)
    -- Does the module have an ouptu with the given name?
    moduleHasOutput m n = n `elem` snd m

-- | Modules are named with Strings
-- (This might be too much type-def-ing, but it does make the later definitions clear.
type ModuleName = String

-- | The different types of modules.
-- I didn't include the button module.  Starting with Broadcast seems fine.
data ModuleType
  = Broadcast
  | FlipFlop
  | Conjunction
  | NoOp
  deriving (Eq, Ord, Show)

-- | The definition of a module consists of its type and output connections
type ModuleDef = (ModuleType, [String])

-- | Map from a module's name to its definition
type Modules = M.Map ModuleName ModuleDef

-- | The two kinds of pulses
data PulseType
  = Low
  | High
  deriving (Eq, Ord, Show)

-- | The state of a FlipFlop
data OnOrOff
  = On
  | Off
  deriving (Eq, Ord, Show)

-- | The state of one module
data ModuleState
  = -- Broadcast modules just are -- no state
    BroadcastState
  | -- FlipFlops are either on or off
    FlipFlopState OnOrOff
  | -- Conjunctions remember the last pulse from each input
    ConjunctionState (M.Map String PulseType)
  | -- NoOps have no state
    NoOpState
  deriving (Eq, Ord, Show)

-- | Counts of activity in the machine
data MachineCounts = MachineCounts
  { mcLow :: Int, -- number of Low pulses delivered
    mcHigh :: Int, -- number of High pulses delivered
    mcLowToRx :: Int -- number of Low pulses delivered to "rx"
  }
  deriving (Show)

-- | The state of the whole machine consists of a state for each module
data MachineState = MachineState
  { machineModules :: M.Map ModuleName ModuleState,
    machineCounts :: MachineCounts
  }
  deriving (Show)

-- | Initial state of the counters
initialCounts :: MachineCounts
initialCounts =
  MachineCounts
    { mcLow = 0,
      mcHigh = 0,
      mcLowToRx = 0
    }

-- | Update the low/high counts kept in the machine state
updateCounts :: PulseType -> String -> MachineCounts -> MachineCounts
updateCounts p n MachineCounts {mcLow = low, mcHigh = high, mcLowToRx = lowRx} =
  MachineCounts
    { mcLow = if p == Low then low + 1 else low,
      mcHigh = if p == High then high + 1 else high,
      mcLowToRx = if p == Low && n == "rx" then lowRx + 1 else lowRx
    }

-- | Convert an input file into a set of module definitions
parse :: String -> Modules
parse =
  M.fromList . map parseLine . lines

-- | Parse one input line
--
-- >>> parseLine "broadcaster -> a, b, c"
-- ("broadcaster",(Broadcast,["a,","b,","c"]))
--
-- >>> parseLine "%b -> con"
-- ("b",(FlipFlop,["con"]))
parseLine :: String -> (String, (ModuleType, [String]))
parseLine s =
  (modName, (modType, outputs))
  where
    [lhs, rhs] = splitOn " -> " . filter (/= ',') $ s
    (modName, modType) = case lhs of
      "broadcaster" -> ("broadcaster", Broadcast)
      ('%' : name) -> (name, FlipFlop)
      ('&' : name) -> (name, Conjunction)
    outputs = words rhs

-- | An event that is a pulse going from one module to another.
type PulseEvent =
  ( String, -- the name of the module the pulse is coming from
    String, -- the name of the module the pulse is going to
    PulseType -- low or high pulse?
  )

-- | What's the initial state of the machine?
initialState :: Modules -> MachineState
initialState modules =
  MachineState {machineModules = moduleStates, machineCounts = initialCounts}
  where
    moduleStates = M.fromList . map modState . M.toList $ modules
    modState (name, (modType, _)) =
      case modType of
        Broadcast -> (name, BroadcastState)
        FlipFlop -> (name, FlipFlopState Off)
        Conjunction -> (name, ConjunctionState (conState name))
        NoOp -> (name, NoOpState)
    conState = M.fromList . map (,Low) . S.toList . (modToInputs M.!)
    modToInputs = mtsTranspose . M.fromList . map (second (S.fromList . snd)) . M.toList $ modules

-- | Push the button.  Return the state after all events have been processed.
-- Events are (fromModuleName, toModuleName, pulseLevel)
pushButton :: Modules -> MachineState -> MachineState
pushButton modules start =
  processEvents oneEvent start ("button", "broadcaster", Low)
  where
    oneEvent s e =
      (MachineState {machineModules = mm', machineCounts = mc'}, newEvents)
      where
        MachineState {machineModules = mm, machineCounts = mc} = s
        (fromName, toName, level) = e
        -- the definition of the module to run has the list ouf outgoing connections
        (_, outgoing) = M.findWithDefault (NoOp, []) toName modules
        -- current state of that module
        ms = M.findWithDefault NoOpState toName mm
        -- let the module process the event
        (ms', maybePulse) = runModule ms fromName level
        -- create the outgoing events
        newEvents = case maybePulse of
          Nothing -> []
          Just p -> map (toName,,p) outgoing
        -- update this module's state in the machine state
        mm' = M.insert toName ms' mm
        -- update the pulse counts
        mc' = updateCounts level toName mc

-- mc' = case maybePulse of
--   Nothing -> mc
--   Just p -> updateCounts p (length outgoing) mc

-- | One module processes one event
--
-- >>> runModule (FlipFlopState Off) "alfa" Low
-- (FlipFlopState On,Just High)
runModule ::
  ModuleState -> -- the state of the module before running
  String -> -- the name of the module the pulse came from
  PulseType -> -- the type of the incoming pulse
  ( ModuleState, -- the state of the module after processing
    Maybe PulseType -- the pulse that is sent out
  )
runModule ms fromName pulseType =
  case ms of
    BroadcastState -> (ms, Just pulseType)
    FlipFlopState ffs -> case pulseType of
      High -> (ms, Nothing)
      Low -> case ffs of
        Off -> (FlipFlopState On, Just High)
        On -> (FlipFlopState Off, Just Low)
    ConjunctionState sources ->
      (ConjunctionState sources', Just pulseType')
      where
        sources' = M.insert fromName pulseType sources
        allHigh = all (== High) . M.elems $ sources'
        pulseType' = if allHigh then Low else High
    NoOpState -> (ms, Nothing)

-- | Process and queue events until they are all done.
-- Events are processed in the order generated.
--
-- >>> processEvents (\s e -> (s ++ [e], [1 .. (e - 1)])) [] 4
-- [4,1,2,3,1,1,2,1]
processEvents ::
  (s -> e -> (s, [e])) -> -- function that takes a state and an event, and returns the new state, and any events that were generated
  s -> -- Initial state
  e -> -- The first event
  s -- Return the final state
processEvents f s e =
  go s [e]
  where
    go s [] = s
    go s (e : es) =
      go s' (es ++ es')
      where
        (s', es') = f s e

-- | A MapToSet holds a set of values for each key
type MapToSet k v = M.Map k (S.Set v)

-- | An empty MapToSet
mtsEmpty :: MapToSet k v
mtsEmpty = M.empty

-- | Insert a value into a the set for the given key.
mtsInsert :: (Ord k, Ord v) => k -> v -> MapToSet k v -> MapToSet k v
mtsInsert k v m =
  M.insert k newSet m
  where
    newSet = S.insert v oldSet
    oldSet = M.findWithDefault S.empty k m

-- | Create a MapToSet from a list of key-value pairs
--
-- >>> mtsFromList [(1, 'A'), (1, 'B'), (2, 'B')]
-- fromList [(1,fromList "AB"),(2,fromList "B")]
mtsFromList :: (Ord k, Ord v) => [(k, v)] -> MapToSet k v
mtsFromList =
  foldl' add mtsEmpty
  where
    add m (k, v) = mtsInsert k v m

-- | List of (k, v) pairs in a MapToSet
mtsToList :: MapToSet k v -> [(k, v)]
mtsToList =
  concatMap doOneSet . M.toList
  where
    doOneSet (k, s) = map (k,) (S.toList s)

-- | Reverse the direction of a MapToSet
--
-- >>> mtsTranspose (mtsFromList [(1, 'A'), (1, 'B'), (2, 'B')])
-- fromList [('A',fromList [1]),('B',fromList [1,2])]
mtsTranspose :: (Ord a, Ord b, Show a, Show b) => MapToSet a b -> MapToSet b a
mtsTranspose = mtsFromList . map swap . mtsToList
