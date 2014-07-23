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

