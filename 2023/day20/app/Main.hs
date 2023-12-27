{-# LANGUAGE TupleSections #-}

module Main where

import Advent (run)
import qualified Data.Foldable as F
import Data.Function ((&))
import qualified Data.Graph.Wrapper as G
import Data.List (findIndex, foldl', sort)
import Data.List.Split (chunk, splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Tuple (swap)
import Data.Tuple.Extra (first, second)
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

-- | Run part2 only on machines that have an "rx" module.
part2 :: Modules -> Int
part2 modules =
  case rxSourceModule of
    Nothing -> -1
    Just rsm -> fst . head . moduleOutput modules $ rsm
  where
    -- The unique module that outputs to rx, if there is one
    rxSourceModule = onlyOrNothing . filter (`moduleHasOutput` "rx") . M.elems $ modules
    -- Does the module have an output with the given name?
    moduleHasOutput m n = n `elem` snd m

-- | Returns the stream of events (time, pulseType) output by a module
moduleOutput :: Modules -> ModuleDef -> [(Int, PulseType)]
moduleOutput _ _ = [(5, Low)]

-- | Solve part2 (I think) in a way that takes waaay to long to run.
part2TooSlow :: Modules -> Int
part2TooSlow modules =
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

part2Faster :: Modules -> Int
part2Faster modules = 0

-- | A group of modules with at most one input and at most one output.
data ModuleGroup = ModuleGroup
  { mgEdgeIn :: (ModuleName, ModuleName), -- The edge feeding into the group
    mgEdgeOut :: (ModuleName, ModuleName), -- The edge leaving the group
    mgModules :: Modules -- The modules in the group
  }
  deriving (Show)

mgOutputModule :: ModuleGroup -> ModuleName
mgOutputModule = fst . mgEdgeOut

mgOutputTo :: ModuleGroup -> ModuleName
mgOutputTo = snd . mgEdgeOut

-- | Groups modules into strongly connected components.
-- Includes only groups that have exactly one edge coming in, and one going out.
-- In the given problem, this is true for every group except for ["broacaster"] and ["rx"].
--
-- Returns a map from the name of the group's output module to the group.
-- This lets the consumers of the group's output find the group.
groupModules :: Modules -> M.Map ModuleName ModuleGroup
groupModules modules =
  M.fromList . map (\g -> (mgOutputModule g, g)) . mapMaybe makeGroup $ sccs
  where
    -- Make a group of modules with the given names
    makeGroup names =
      mkGroup <$> modIn names <*> modOut names
      where
        mkGroup ei eo =
          ModuleGroup
            { mgEdgeIn = ei,
              mgEdgeOut = eo,
              mgModules = M.fromList . map (\n -> (n, modules M.! n)) $ names
            }

    -- The edge leading into a connected group
    modOut = onlyOrNothing . edgesOut graph

    -- The edge leading into a connected group
    modIn = onlyOrNothing . map swap . edgesOut inverseGraph

    -- Strongly connected components.
    -- A list of lists of module names.  Each of the inner lists is a connected component.
    sccs = map F.toList . F.toList . G.stronglyConnectedComponents $ graph

    -- A directed graph with edges from modules to the modules the send to.
    graph = moduleGraph modules

    -- A directed graph with edges from modules to the modules they receive input from
    inverseGraph = G.transpose graph

-- | All edges leaving a module group.
-- Result is a list: [(nodeInGroup, nodeOutsideGroup)]
edgesOut :: (Ord a) => G.Graph a a -> [a] -> [(a, a)]
edgesOut graph group =
  filter (not . (`elem` group) . snd) pairs
  where
    -- All edges starting inside the group
    pairs = concatMap (successorPairs graph) group

-- | A graph of the names of all modules, with edges for the flow of pulses
moduleGraph :: Modules -> G.Graph ModuleName ModuleName
moduleGraph modules =
  G.fromListSimple (modAndOutputs ++ leavesAndNoOutputs)
  where
    -- A list of pairs: (moduleName, [outputsOfModuleName])
    modAndOutputs = map (\(name, (_, outs)) -> (name, outs)) . M.toList $ modules

    -- A list of pairs: (leafName, [])
    leavesAndNoOutputs = map (,[]) leaves

    -- All of the module names that have definitions
    nonLeaves = S.fromList . map fst $ modAndOutputs

    -- All of the module names that are listed as outputs, but don't have definitions.
    leaves = unique . concatMap (filter (not . (`S.member` nonLeaves)) . snd) $ modAndOutputs

-- | Unique values in a list
unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

-- | Returns the single value in the list, or Nothing if there are none or two or more.
onlyOrNothing :: [a] -> Maybe a
onlyOrNothing [x] = Just x
onlyOrNothing _ = Nothing

-- | Move a Maybe inside a pair.  TODO: find a library function that does this
unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
unzipMaybe Nothing = (Nothing, Nothing)
unzipMaybe (Just (a, b)) = (Just a, Just b)

-- | Given a node n in a graph g, return a list of [(n, s)] for all successors s.
successorPairs :: (Ord i) => G.Graph i v -> i -> [(i, i)]
successorPairs g n = map (n,) (G.successors g n)

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
  | Collector
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
  | -- Collectors hold a pulse type
    CollectorState (Maybe PulseType)
  deriving (Eq, Ord, Show)

type ModuleStates = M.Map ModuleName ModuleState

-- | Counts of activity in the machine
data MachineCounts = MachineCounts
  { mcLow :: Int, -- number of Low pulses delivered
    mcHigh :: Int, -- number of High pulses delivered
    mcLowToRx :: Int -- number of Low pulses delivered to "rx"
  }
  deriving (Show)

-- | The state of the whole machine consists of a state for each module
data MachineState = MachineState
  { machineModules :: ModuleStates,
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
        -- the definition of the module to run has the list of outgoing connections
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

-- | An infinite sequence of events, which repeats in a cycle after some point.
data Cycle e = Cycle
  { cycInitial :: [e], -- events that happen before the repeating starts
    cycRepeat :: [e] -- sequence that then repeats forever
  }
  deriving (Show)

-- | Find a Cycle in a sequence events generated by something with state.
-- Assumes that a given state will always result in the same sequence of events.
--
-- >>> findCycle 0 [('A', 1), ('B', 2), ('C', 3), ('D', 2)]
-- Cycle {cyc2Initial = "AB", cyc2Repeat = "CD"}
findCycle ::
  (Ord s) =>
  s -> -- initial state
  [(e, s)] -> -- an event, and the state that follows it
  Cycle e -- the cycle
findCycle s0 =
  go (M.insert s0 0 M.empty) 1 []
  where
    go history i revList ((e, s) : ess) =
      case M.lookup s history of
        Just j ->
          Cycle
            { cycInitial = take j list,
              cycRepeat = drop j list
            }
          where
            list = reverse (e : revList)
        Nothing -> go (M.insert s i history) (i + 1) (e : revList) ess

-- | The infinite sequence of events defined by a Cycle
--
-- >>> take 10 . runCycle $ findCycle 0 [('A', 1), ('B', 2), ('C', 3), ('D', 2)]
-- "ABCDCDCDCD"
runCycle :: Cycle e -> [e]
runCycle c =
  cycInitial c ++ cycle (cycRepeat c)

-- | Generates the events in a cycle, and implements Ord for state comparisons.
data CycleGenerator e
  = CycleGenerator [e] [e]
  deriving (Eq, Ord, Show)

-- | Make a CycleGenerator from a Cycle
cycleGen :: Cycle e -> CycleGenerator e
cycleGen c = CycleGenerator (cycInitial c) (cycRepeat c)

-- | Generate the next event from a CycleGenerator, and the updated generator.
cycleGenNext :: CycleGenerator e -> (e, CycleGenerator e)
cycleGenNext (CycleGenerator [] rs) = cycleGenNext (CycleGenerator rs rs)
cycleGenNext (CycleGenerator (e : es) rs) = (e, CycleGenerator es rs)

-- | Compute the Cycle that's output by the given ModuleGroup
evalModuleGroup :: M.Map ModuleName ModuleGroup -> ModuleName -> Cycle (Int, PulseType)
evalModuleGroup defs name = findCycle 1 [((1, Low), 1)] -- TODO

-- modGroup (cycleGen, modStates)

type TimedPulse = (Int, PulseType)

type GroupState = (CycleGenerator TimedPulse, ModuleStates, Int)

-- | Advance a module group into its next state
advanceGroup :: ModuleGroup -> GroupState -> (GroupState, Maybe TimedPulse)
advanceGroup mg (cg, moduleStates, tsp) =
  ((cg', moduleStates', tsp + dt), Nothing) -- TODO time and pulse
  where
    -- The definitions of the modules in the group
    modules = mgModules mg
    -- Where the output of the group goes
    (_, out) = mgEdgeOut mg
    -- The next pulse to send.  `dt` is the delta time, `pt` is the pulse type
    ((dt, pt), cg') = cycleGenNext cg
    -- The pulse goes into the module group on the one inbound edge
    (firstEventFrom, firstEventTo) = mgEdgeIn mg
    -- The event to send in
    firstEvent = (firstEventFrom, firstEventTo, pt)
    -- Process the event and things cascading from it, producing the new module states
    moduleStates' = processEvents oneEvent moduleStates firstEvent
    -- How to handle one event
    oneEvent s (fromName, toName, level) =
      (s', newEvents)
      where
        -- The definition of the module to run has the list of outgoing connections
        (_, outgoing) = M.findWithDefault (Collector, []) toName modules
        -- the current state of that module
        ms = M.findWithDefault (CollectorState Nothing) toName s
        -- run the module
        (ms', maybePulse) = runModule ms fromName level
        -- create the outgoing events
        newEvents = case maybePulse of
          Nothing -> []
          Just p -> map (toName,,p) outgoing
        -- store the new module state
        s' = M.insert toName ms' moduleStates

-- | Process one event in a set of modules, producing a list of subsequent events.
oneEvent :: Modules -> ModuleStates -> PulseEvent -> (ModuleStates, [PulseEvent])
oneEvent modules moduleStates (fromName, toName, level) =
  (moduleStates', newEvents)
  where
    -- The definition of the module to run has the list of outgoing connections
    (_, outgoing) = modules M.! toName
    -- The current state of that module
    ms = moduleStates M.! toName
    -- Run the module
    (ms', maybePulse) = runModule ms fromName level
    -- Create the outgoing events
    newEvents = case maybePulse of
      Nothing -> []
      Just p -> map (toName,,p) outgoing
    -- store the new module state
    moduleStates' = M.insert toName ms' moduleStates
