import Data.List

--
-- main:
--      * Reads from the standard input
--      * Makes a RoadSystem out of it
--      * Prints out the shortest path
--

main = do
    contents <- getContents
    let threes     = groupsOf 3 $ arrayFor contents
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path       = optimalPath roadSystem
        pathString = stringFor path
        pathTime   = timeFor path

    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime
-- runhaskell fromHeathrowToLondon.hs < paths.txt
--      --> The best path to take is: BCACBBC
--      --> Time taken: 75


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
    let initialTimeForPathA = timeFor pathA
        initialTimeForPathB = timeFor pathB

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
    in  if timeFor bestAPath <= timeFor bestBPath
            then reverse bestAPath
            else reverse bestBPath
-- optimalPath heathrowToLondon --> [ (B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0) ]


--
-- groupsOf: takes a list and splits it into groups of the same size
--

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
-- groupsOf 0 []      --> *** Exception: Prelude.undefined
-- groupsOf 3 []      --> []
-- groupsOf 3 [1..10] --> [[1,2,3],[4,5,6],[7,8,9],[10]]


--
-- helpers: timeFor, stringFor, arrayFor
--

timeFor :: Path -> Int
timeFor path = sum (map snd path)

stringFor :: Path -> String
stringFor path = concat $ map (show . fst) path

arrayFor :: String -> [Int]
arrayFor contents = map read $ lines contents

