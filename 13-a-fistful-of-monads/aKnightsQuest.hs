--
-- We want to find out if a Knight can reach a certain position in 3 moves
--

import Control.Monad

--
-- Type Definitions
--
type Col = Int
type Row = Int
type KnightPos = (Col, Row)


--
-- Functions
--

--
-- moveKnight: Produces all possible (inside the board) moves from current position
--

-- Using lists as monads
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [ (c+2,r-1), (c+2,r+1), (c-2,r-1), (c-2,r+1)
               , (c+1,r-2), (c+1,r+2), (c-1,r-2), (c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')
-- moveKnight (6,2) --> [(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]
-- moveKnight (8,1) --> [(6,2),(7,3)]

-- Using filter instead of lists as monads
moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c,r) = filter onBoard
    [ (c+2,r-1), (c+2,r+1), (c-2,r-1), (c-2,r+1)
    , (c+1,r-2), (c+1,r+2), (c-1,r-2), (c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]
-- moveKnight' (6,2) --> [(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]
-- moveKnight' (8,1) --> [(6,2),(7,3)]


--
-- in3: Produces all the positions you can reach in 3 moves, given an initial position
--

-- Using 'do notation'
in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

-- Using '>>='
in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight


--
-- canReachIn3: Given an initial and final position, evaluates if you can get there in 3 moves
--

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
-- (6,2) `canReachIn3` (6,1) --> True
-- (6,2) `canReachIn3` (7,3) --> False

