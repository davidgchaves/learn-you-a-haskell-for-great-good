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

