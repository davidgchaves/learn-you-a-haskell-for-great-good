-- A section is composed of:
--      * Road A
--      * Road B
--      * Road C (Crossing Road between Road A and Road B)
-- Section is a simple Algebraic Data Type
-- that holds 3 integers for the durations of its 3 road parts
data Section = Section { getA :: Int, getB :: Int, getC :: Int }
    deriving (Show)

-- A road system is splited into sections
-- RoadSystem is a Type Synonym represented as a list of Sections
type RoadSystem = [Section]

-- Heathrow to London represented as a RoadSystem
heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section  5 90 20
                   , Section 40  2 25
                   , Section 10  8  0]

-- optimalPath should have the following type:
-- optimalPath :: RoadSystem -> Path
--
-- Path can be represented as a list of
--      * Roads (A, B or C)
--      * Durations
--
-- Let's introduce a Label type (a simple enumeration)
data Label = A | B | C
    deriving (Show)

-- Path is simply a Type Synonym of Labels and Durations
type Path = [(Label, Int)]

-- As an example, the heathrowToLondon solution should be:
heathrowToLondonSolution = [(B,10), (C,30), (A,5), (C,20), (B,2), (B,8)]


--
-- roadStep: Check the optimal paths on A and B so far and the current section
--           to produce the new optimal paths on A and B
--           NOTE: THE PATHS ARE REVERSED. READ THEM FROM RIGHT LO LEFT
--

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let initialTimeForPathA = sum (map snd pathA)
        initialTimeForPathB = sum (map snd pathB)

        forwardTimeToA = initialTimeForPathA + a
        crossTimeToA   = initialTimeForPathB + b + c
        forwardTimeToB = initialTimeForPathB + b
        crossTimeToB   = initialTimeForPathA + a + c

        newPathToA = if forwardTimeToA <= crossTimeToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        newPathToB = if forwardTimeToB <= crossTimeToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA

    in (newPathToA, newPathToB)
-- roadStep ([], []) (head heathrowToLondon) --> ( [(C,30),(B,10)], [(B,10)] )


--
-- optimalPath
--

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath
-- optimalPath heathrowToLondon --> [ (B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0) ]

