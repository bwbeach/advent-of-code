{-# LANGUAGE TupleSections #-}

-- | Advent of Code, 2023.  Day 20.
--
-- I found this one unsatisfactory, because I failed to write a general
-- solution that works with any input.  Looking at the graph of modules
-- drawn by graphviz, I saw that there were four big clumps.  It turns out
-- that each of the clumps repeats with a period that is a prime number.
-- Multiplying those numbers produced the answer.
module Main where

import Advent (only, run)
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
    -- Add a dummy count
    & (,EventCounts 0 0,[])
    -- Push the button as many times as needed, producing a list of (machineState, eventCount)
    & iterate (pushHelper modules)
    -- Take the results of the first 1000 pushes, without the first dummy counts
    & take 1000 . drop 1
    -- Extract the counts
    & map (\(_, ec, _) -> ec)
    -- Add them up
    & foldl' addCounts (EventCounts 0 0)
    -- Multiply the low count and high count
    & (\(EventCounts a b) -> a * b)
  where
    pushHelper modules (s, _, _) = pushButton modules s

-- | Run part2 only on machines that have an "rx" module.
part2 :: Modules -> Int
part2 modules =
  case rxSourceModule of
    Nothing -> -1 -- the two tests don't have an rx module, so part 2 doesn't make sense
    Just rsm -> product . traceShowId . map groupPeriod . traceShowId $ bigGroupNames
  where
    -- The unique module that outputs to rx, if there is one
    rxSourceModule = onlyOrNothing . filter (`moduleHasOutput` "rx") . M.elems $ modules
    -- Does the module have an output with the given name?
    moduleHasOutput m n = n `elem` snd m
    -- Modules grouped into strongly connected components
    groups = groupModules modules
    -- All groups with 4 or more modules
    bigGroupNames = map (fst . mgEdgeOut) bigGroups
    bigGroups = filter isBigGroup . M.elems $ groups
    isBigGroup = (4 <) . M.size . mgModules
    -- Cycle period of a group
    groupPeriod =
      sum . map fst . cycRepeat . groupOutput groups

-- | A group of modules with at most one input and at most one output.
data ModuleGroup = ModuleGroup
  { mgEdgeIn :: (ModuleName, ModuleName), -- The edge feeding into the group
    mgEdgeOut :: (ModuleName, ModuleName), -- The edge leaving the group
    mgModules :: Modules -- The modules in the group
  }
  deriving (Show)

type ModuleGroups = M.Map ModuleName ModuleGroup

mgOutputModule :: ModuleGroup -> ModuleName
mgOutputModule = fst . mgEdgeOut

-- | Groups modules into strongly connected components.
-- Includes only groups that have exactly one edge coming in, and one going out.
-- In the given problem, this is true for every group except for ["broacaster"] and ["rx"].
--
-- Returns a map from the name of the group's output module to the group.
-- This lets the consumers of the group's output find the group.
groupModules :: Modules -> ModuleGroups
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
  | -- Conjunctions remembers the last pulse from each input and the last pulse type sent
    ConjunctionState (M.Map String PulseType) (Maybe PulseType)
  | -- Collectors hold a pulse type
    CollectorState (Maybe PulseType)
  deriving (Eq, Ord, Show)

-- | The states of all of the modules in a machine
type ModuleStates = M.Map ModuleName ModuleState

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

-- | Count of events delivered
data EventCounts
  = EventCounts Int Int
  deriving (Show)

addCounts :: EventCounts -> EventCounts -> EventCounts
addCounts (EventCounts a b) (EventCounts c d) = EventCounts (a + c) (b + d)

bumpCounts :: EventCounts -> PulseType -> EventCounts
bumpCounts (EventCounts a b) Low = EventCounts (a + 1) b
bumpCounts (EventCounts a b) High = EventCounts a (b + 1)

-- | What's the initial state of the machine?
initialState :: Modules -> ModuleStates
initialState modules =
  moduleStates
  where
    moduleStates = M.fromList . map modState . M.toList $ modules
    modState (name, (modType, _)) =
      case modType of
        Broadcast -> (name, BroadcastState)
        FlipFlop -> (name, FlipFlopState Off)
        Conjunction -> (name, ConjunctionState (conState name) Nothing)
    conState = M.fromList . map (,Low) . S.toList . (modToInputs M.!)
    modToInputs = mtsTranspose . M.fromList . map (second (S.fromList . snd)) . M.toList $ modules

-- | Push the button.  Return the state after all events have been processed.
pushButton :: Modules -> ModuleStates -> (ModuleStates, EventCounts, [PulseType])
pushButton modules start = propagateEvent modules start ("button", "broadcaster", Low)

-- | Propagate one event through a group of modules.  Return the state after all events have been processed.
-- Events are (fromModuleName, toModuleName, pulseLevel)
-- State carried across events: (ModuleStates, allEventCounts, eventCountsToUndefined)
propagateEvent :: Modules -> ModuleStates -> (ModuleName, ModuleName, PulseType) -> (ModuleStates, EventCounts, [PulseType])
propagateEvent modules start =
  processEvents oneEvent (start, EventCounts 0 0, [])
  where
    oneEvent (s, ec, ext) e@(_, toName, level) =
      if M.member toName modules
        then oneEventToModule (s, ec, ext) e
        else ((s, bumpCounts ec level, ext ++ [level]), [])

    oneEventToModule (s, ec, ext) (fromName, toName, level) =
      ((s', ec', ext), newEvents)
      where
        -- the definition of the module to run has the list of outgoing connections
        (_, outgoing) = modules M.! toName
        -- current state of that module
        ms = s M.! toName
        -- let the module process the event
        (ms', maybePulse) = runModule ms fromName level
        -- create the outgoing events
        newEvents = case maybePulse of
          Nothing -> []
          Just p -> map (toName,,p) outgoing
        -- update this module's state in the machine state
        s' = M.insert toName ms' s
        -- update the pulse counts
        ec' = bumpCounts ec level

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
    ConjunctionState sources prev ->
      (ConjunctionState sources' (Just pulseType'), Just pulseType')
      where
        sources' = M.insert fromName pulseType sources
        allHigh = all (== High) . M.elems $ sources'
        pulseType' = if allHigh then Low else High

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

type TimedPulses = (Int, [PulseType])

type GroupState = (CycleGenerator TimedPulses, ModuleStates, Int)

-- | The Cycle of timed pulses produced by a module group
--
-- Keeps sending events into the group until a cycle is found in the output.
--
-- The state consists of: (CycleGenerator, ModuleStates)
--
-- The output of `iterate` is a sequence of:
--    (timeSinceLastPulse, CycleGenerator, ModuleStates, [PulseLevel])
-- The output of each iteration is maybe a TimedPulses, pluse the above state.
groupOutput :: ModuleGroups -> String -> Cycle TimedPulses
groupOutput _ "broadcaster" = Cycle {cycInitial = [], cycRepeat = [(1, [Low])]}
groupOutput groups groupName =
  findCycle (inputGen, modStates) (extractEvents seq)
  where
    -- Get info from the group definition
    ModuleGroup
      { mgEdgeIn = (inputFrom, inputTo),
        mgEdgeOut = edgeOut,
        mgModules = modules
      } = groups M.! groupName
    -- Get the input generator
    inputGen = cycleGen (groupOutput groups inputFrom)
    -- Get the initial state of the modules in the group
    modStates = initialState modules
    -- start for iteration
    start = (0, inputGen, modStates, [])
    -- sequence of states and outputs for the group
    seq = iterate next start
    -- given one item in the sequence, produce the next one
    next :: (Int, CycleGenerator TimedPulses, ModuleStates, [PulseType]) -> (Int, CycleGenerator TimedPulses, ModuleStates, [PulseType])
    next (_, gen, mods, _) =
      (dt, gen', mods', out)
      where
        (mods', _, out) = propagateEvent modules mods eventIn
        eventIn = (inputFrom, inputTo, only levels)
        ((dt, levels), gen') = cycleGenNext gen
    -- extract (state, event) pairs from the infinite sequence
    extractEvents =
      go 0
      where
        go timeSoFar ((dt, gen, mods, out) : es) =
          case out of
            [] -> go (timeSoFar + dt) es
            _ -> ((timeSoFar + dt, out), (gen, mods)) : go 0 es
