# 2023 Day 21: Step Counter

The problem today is to find all the possible locations an elf could
be in after taking N steps up/down/left right in a rectangular grid
while avoiding rocks (`#`).

## Part 1

### First Pass
The grid in part 1 is finite, and the numbers are small, so a straightforward solution works fine.  The set of possible locations at time T is represented
as a `Set Point`.  The function `next` takes the grid and a starting set and produces the set of possible locations at the next time.

```haskell
-- | Sequence of sets containing all possible positions at one time
allPossible :: (GenericGrid g) => g -> S.Set Point -> [S.Set Point]
allPossible problem =
  iterate next
  where
    -- Given a set of Os, where are the Os in the next iteration?
    next = S.fromList . filter isGarden . candidates

    -- Candidate locations where Os might be
    candidates = concatMap neighbors . S.toList

    -- Neighbors of a point
    neighbors p = map (p +) directions

    -- Deltas to neighbors
    directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

    -- Is the given location a garden plot?
    isGarden p = ggGet p problem `elem` ".S"
```

The answer to part 1 is then the size of the set after 64 iterations.

```haskell
part1 :: Problem -> Int
part1 problem =
  S.size . (!! 64) $ allPossible problem initial
  where
    -- The initial set of locations is the one place where the 'S' is
    initial = S.singleton . findS $ problem

-- | Coordinates of the 'S' on the grid
findS :: Grid -> Point
findS = only . map fst . filter ((== 'S') . snd) . M.toList . gridMap
```

The number of answers is proportional to the area of a shape of radius n, or O(n ^ 2).  This algorithm visits each of the locations each time through as the size o the set scales up.  Each visit is an O(log n) lookup in a `Set`.  That makes the whole thing O(n^3 log n).

### Second Pass

Can we do better?  I think so.

What if each step produces just the set of newly visited locations?  Say we have the last two sets: `mostRecent` is the set of locations just added, and `prior` is the set of locations before that.  The next set will be the neighbors of `mostRecent` that aren't in `prior`

Each time through, we'll process a set that is O(n) with O(log n) lookups, resulting in O(n^2 log n).  Not great, but not terrible for an answer of size O(n^2).

One thing to note is that there a two sets of visited locations: those that are an even number of steps away, and those that are an odd number of steps away.  Once the grid is fully visited, the set of nodes possible alternates between two different sets.

As the answers are being visited, this code keeps two counts: one for the current set, and one for the other set.

```haskell
part1 :: Problem -> Int
part1 problem =
  total . (!! 64) $ allPossible2 problem (initial, S.empty, 1, 0)
  where
    -- The initial set of locations is the one place where the 'S' is
    initial = S.singleton . findS $ problem

    -- The total count from one tuple
    total (_, _, t, _) = t
    
-- | Sequence of states: (mostRecent, prior, mostRecentCount, priorCount)
-- `mostRecent` is the set of locations added in the last pass
-- `prior` is the set of locations added in the pass before that
-- `mostRecentCount` is the sum of sizes of `mostRecent` and all previous sets of the same parity.
-- `priorCount` is the sum of sizes of `prior` and all previous sets of the same parity.
allPossible2 :: (GenericGrid g) => g -> (S.Set Point, S.Set Point, Int, Int) -> [(S.Set Point, S.Set Point, Int, Int)]
allPossible2 problem =
  iterate next
  where
    -- What's the next tuple in the sequence?
    next (s1, s2, c1, c2) =
      (s0, s1, c2 + S.size s0, c1)
      where
        -- What locations get added in this round?
        s0 = S.fromList . filter isGarden . filter (not . (`S.member` s2)) . candidates $ s1

        -- Candidate locations next to the previous set
        candidates = concatMap neighbors . S.toList

        -- Neighbors of a point
        neighbors p = map (p +) directions

        -- Deltas to neighbors
        directions = [V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1)]

        -- Is the given location a garden plot?
        isGarden p = ggGet p problem `elem` ".S"
```

## Part 2

Yikes!  In part 2, the grid becomes infinite by tiling the given grid, and the number of iterations becomes 26501365.  On an infinite grid, the size of the answer will grow by O(n^2).  So after ~26 million iterations, the answer will be on the order of 500 trillion.  Even an O(n) algorithm that touches them all will be far to slow.

### Setup

The grid for Part 2 is infinite, so we'll need a way to represent that.  First, let's generalize grids to be a class with just a "get" operation, which is all we've needed so far.  The instance for the existing `Grid` class in my Advent library is easy.

```haskell
-- | A grid of cells on an integer plane, holding Chars.
class GenericGrid g where
  ggGet :: Point -> g -> Char

instance GenericGrid Grid where
  ggGet = gridGet
```

This adds a new instance that is an infinite grid created by tiling an existing grid.

```haskell
-- | An infinite grid creating by tiling a finite grid
data InfiniteGrid
  = InfiniteGrid (Int -> Int) (Int -> Int) Grid

-- | Create an infinite grid
infiniteGrid :: Grid -> InfiniteGrid
infiniteGrid g =
  InfiniteGrid mapX mapY g
  where
    -- Function to take an x value and map it to an x in the finite grid
    mapX x = x0 + (x - x0) `mod` width
    -- Function to take a y value and map it to a y in the finite grid
    mapY y = y0 + (y - y0) `mod` height

    -- Size of the tiled grid
    (V2 x0 y0, V2 x1 y1) = gridBounds g
    width = x1 - x0
    height = y1 - y0

instance GenericGrid InfiniteGrid where
  ggGet (V2 x y) (InfiniteGrid mapX mapY g) = gridGet (V2 (mapX x) (mapY y)) g
```

### First Idea

Could this be like Day 9, "Mirage Maintenance", with a pattern to the growth expressed as differences in the sequence of sizes?  The answers grow with `n^2`, so the differences between them should grow with `n`, and the differences between those might be a constant pattern?

This will print the three sequences:

```haskell
part2 :: Problem -> Int
part2 problem =
  (!! 1) . traceShowId . deltas . traceShowId . deltas . traceShowId . map S.size . take 200 $ allPossible bigProblem initial
  where
    bigProblem = infiniteGrid problem

    -- The initial set of locations is the one place where the 'S' is
    initial = S.singleton . findS $ problem

-- | Sequence of differences between adjacent numbers in a sequence
deltas :: (Num a) => [a] -> [a]
deltas = map (uncurry . flip $ (-)) . pairs
```

This is the output on the test and on the actual input.  I don't see a pattern.

```haskell
[1,2,4,6,9,13,16,22,29,34,43,49,62,68,87,90,108,123,153,161,187,204,238,250,299,304,347,361,421,418,473,484,548,558,646,641,717,725,822,804,891,887,991,987,1121,1098,1216,1209,1351,1309,1437,1410,1561,1534,1721,1671,1839,1809,2004,1930,2107,2049,2255,2197,2445,2360,2586,2525,2781,2667,2901,2804,3073,2976,3293,3165,3457,3357,3682,3520,3819,3675,4015,3871,4265,4086,4452,4305,4707,4489,4861,4662,5081,4882,5361,5123,5571,5369,5856,5574,6027,5765,6271,6009,6581,6276,6814,6549,7129,6775,7317,6984,7585,7252,7925,7545,8181,7845,8526,8092,8731,8319,9023,8611,9393,8930,9672,9257,10047,9525,10269,9770,10585,10086,10985,10431,11287,10785,11692,11074,11931,11337,12271,11677,12701,12048,13026,12429,13461,12739,13717,13020,14081,13384,14541,13781,14889,14189,15354,14520,15627,14819,16015,15207,16505,15630,16876,16065,17371,16417,17661,16734,18073,17146,18593,17595,18987,18057,19512,18430,19819,18765,20255,19201,20805,19676,21222,20165,21777,20559,22101,20912,22561,21372,23141,21873,23581,22389,24166,22804]
[1,2,2,3,4,3,6,7,5,9,6,13,6,19,3,18,15,30,8,26,17,34,12,49,5,43,14,60,-3,55,11,64,10,88,-5,76,8,97,-18,87,-4,104,-4,134,-23,118,-7,142,-42,128,-27,151,-27,187,-50,168,-30,195,-74,177,-58,206,-58,248,-85,226,-61,256,-114,234,-97,269,-97,317,-128,292,-100,325,-162,299,-144,340,-144,394,-179,366,-147,402,-218,372,-199,419,-199,479,-238,448,-202,487,-282,453,-262,506,-262,572,-305,538,-265,580,-354,542,-333,601,-333,673,-380,636,-336,681,-434,639,-412,704,-412,782,-463,742,-415,790,-522,744,-499,815,-499,899,-554,856,-502,907,-618,857,-594,934,-594,1024,-653,978,-597,1032,-722,978,-697,1061,-697,1157,-760,1108,-700,1165,-834,1107,-808,1196,-808,1298,-875,1246,-811,1306,-954,1244,-927,1339,-927,1447,-998,1392,-930,1455,-1082,1389,-1054,1490,-1054,1604,-1129,1546,-1057,1612,-1218,1542,-1189,1649,-1189,1769,-1268,1708,-1192,1777,-1362]
[1,0,1,1,-1,3,1,-2,4,-3,7,-7,13,-16,15,-3,15,-22,18,-9,17,-22,37,-44,38,-29,46,-63,58,-44,53,-54,78,-93,81,-68,89,-115,105,-91,108,-108,138,-157,141,-125,149,-184,170,-155,178,-178,214,-237,218,-198,225,-269,251,-235,264,-264,306,-333,311,-287,317,-370,348,-331,366,-366,414,-445,420,-392,425,-487,461,-443,484,-484,538,-573,545,-513,549,-620,590,-571,618,-618,678,-717,686,-650,689,-769,735,-715,768,-768,834,-877,843,-803,845,-934,896,-875,934,-934,1006,-1053,1016,-972,1017,-1115,1073,-1051,1116,-1116,1194,-1245,1205,-1157,1205,-1312,1266,-1243,1314,-1314,1398,-1453,1410,-1358,1409,-1525,1475,-1451,1528,-1528,1618,-1677,1631,-1575,1629,-1754,1700,-1675,1758,-1758,1854,-1917,1868,-1808,1865,-1999,1941,-1915,2004,-2004,2106,-2173,2121,-2057,2117,-2260,2198,-2171,2266,-2266,2374,-2445,2390,-2322,2385,-2537,2471,-2443,2544,-2544,2658,-2733,2675,-2603,2669,-2830,2760,-2731,2838,-2838,2958,-3037,2976,-2900,2969,-3139]

[1,4,9,16,24,34,47,58,76,86,108,125,152,170,196,213,248,270,317,338,387,413,463,494,553,581,643,675,740,772,839,872,939,975,1052,1093,1163,1229,1291,1363,1423,1505,1562,1655,1708,1806,1872,1963,2032,2135,2203,2310,2373,2487,2542,2662,2731,2854,2928,3051,3133,3255,3376,3512,3642,3776,3906,4044,4178,4285,4419,4526,4666,4773,4922,5028,5186,5274,5448,5549,5725,5824,6010,6118,6309,6410,6604,6706,6912,7011,7230,7342,7545,7656,7858,7967,8196,8298,8534,8643,8876,8991,9227,9328,9579,9697,9964,10067,10334,10441,10711,10824,11094,11218,11492,11603,11888,12011,12283,12400,12680,12831,13098,13257,13531,13680,13946,14110,14383,14559,14847,15029,15312,15481,15758,15933,16228,16401,16712,16898,17181,17373,17667,17868,18149,18342,18647,18847,19170,19370,19691,19901,20214,20424,20768,20969,21307,21506,21858,22047,22394,22586,22937,23129,23504,23680,24039,24265,24606,24854,25187,25445,25778,26046,26369,26646,26999,27252,27601,27875,28214,28496,28821,29102,29417,29701,30053,30331,30699,30972,31353,31617,32114,32410,32918,33200,33702,33988,34494,34677]
[3,5,7,8,10,13,11,18,10,22,17,27,18,26,17,35,22,47,21,49,26,50,31,59,28,62,32,65,32,67,33,67,36,77,41,70,66,62,72,60,82,57,93,53,98,66,91,69,103,68,107,63,114,55,120,69,123,74,123,82,122,121,136,130,134,130,138,134,107,134,107,140,107,149,106,158,88,174,101,176,99,186,108,191,101,194,102,206,99,219,112,203,111,202,109,229,102,236,109,233,115,236,101,251,118,267,103,267,107,270,113,270,124,274,111,285,123,272,117,280,151,267,159,274,149,266,164,273,176,288,182,283,169,277,175,295,173,311,186,283,192,294,201,281,193,305,200,323,200,321,210,313,210,344,201,338,199,352,189,347,192,351,192,375,176,359,226,341,248,333,258,333,268,323,277,353,253,349,274,339,282,325,281,315,284,352,278,368,273,381,264,497,296,508,282,502,286,506,183]
[2,2,1,2,3,-2,7,-8,12,-5,10,-9,8,-9,18,-13,25,-26,28,-23,24,-19,28,-31,34,-30,33,-33,35,-34,34,-31,41,-36,29,-4,-4,10,-12,22,-25,36,-40,45,-32,25,-22,34,-35,39,-44,51,-59,65,-51,54,-49,49,-41,40,-1,15,-6,4,-4,8,-4,-27,27,-27,33,-33,42,-43,52,-70,86,-73,75,-77,87,-78,83,-90,93,-92,104,-107,120,-107,91,-92,91,-93,120,-127,134,-127,124,-118,121,-135,150,-133,149,-164,164,-160,163,-157,157,-146,150,-163,174,-162,149,-155,163,-129,116,-108,115,-125,117,-102,109,-97,112,-106,101,-114,108,-102,120,-122,138,-125,97,-91,102,-93,80,-88,112,-105,123,-123,121,-111,103,-103,134,-143,137,-139,153,-163,158,-155,159,-159,183,-199,183,-133,115,-93,85,-75,75,-65,55,-46,76,-100,96,-75,65,-57,43,-44,34,-31,68,-74,90,-95,108,-117,233,-201,212,-226,220,-216,220,-323]
```

### Second Idea

This idea is actually a question: What's the shape of the sets?

They're mostly diamonds, with the edge at a constant manhattan distance from the starting point.  There will occasionally be gaps at the edges in the shadow of rocks.  The gaps quickly get filled in as the expansion continues.  

In the diagrams below, the Os are the locations added in the most recent iteration.

Here's an example with a couple of gaps on the left side:

```
    ##   #O      #   
#        O O#   #    
    #   O   O    #   
#      #    #O       
 ##   O    #  O     #
     O         #     
 #  O      #    O    
  # #      #    #O   
  O#          ##  O  
 O #             # # 
O                   O
 O#               #O 
  O    #          O  
 # #             #   
    O           O    
     O#        O #   
#   #  ### #  O     #
  #    O     O #     
        O   #  #     
       # O O      #  
 #      ##O#      #  
```

Both gaps in the perimiter are filled in on the next iteration when the diamond is one step bigger.

```
       ##  O   #   #   
     ##   # O     #    
 #       O   #   #     
     #  O     O   #    
 #     O#    # O       
  ##  O     #   O    # 
#    O          #O     
  # O       #     O    
#  # #      #    # O   
  O #          ##   O  
 O  #             # #O 
O                     O
 O #               # O 
  O     #           O  
  #O#             #O   
    O             O   #
#    O #         O#    
 #   #O ### #   O    # 
   #   O       O#      
        O    #O #      
        #O   O     #   
  #      ## #      #  #
#        # O    #   #  
```

And then the next step fills in the missing holes:

```
    ##      O ##   ###   
        ## O O  #   #    
      ##  O#  O    #     
  #      O    #O  #      
      # O       O  #    #
  #    O #    #  O       
#  ## O      #    O   #  
##   O           # O     
   #O        #      O    
## O#O#      #    #  O  #
# O  #          ##    O  
#O   #             # # O 
O                       O
 O  #               #  O 
  O      #            O #
#  # #             # O   
    O               O  ##
 #   O  #          #     
  #   # O### #    O   #  
#   #  O         #       
        O     # O#       
         #     O    #    
   #      ## #O     #  # 
 #        #O O   #   #  #
           #O#  ## #  #  
```