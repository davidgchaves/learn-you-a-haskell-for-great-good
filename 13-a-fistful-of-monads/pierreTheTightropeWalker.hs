--
-- Birds and Pole Type Synonyms
--
type Birds = Int
type Pole = (Birds, Birds)

--
-- landLeft: Take a number of Birds and land them on the Left side of the Pole
--           To make Birds fly away just enter a negative number of Birds
--
landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)
-- landLeft 2 (0,0)    --> (2,0)
-- landLeft 1 (3,2)    --> (4,2)
-- landLeft (-1) (3,2) --> (2,2)

--
-- landRight: Take a number of Birds and land them on the Right side of the Pole
--            To make Birds fly away just enter a negative number of Birds
--
landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)
-- landRight 3 (0,0)    --> (0,3)
-- landRight 1 (3,2)    --> (3,3)
-- landRight (-1) (3,2) --> (3,1)

-- Chaining landLeft and landRight:
--  landLeft 1 (landRight 3 (landLeft 2 (0,0))) --> (3,3)
--  meaning 2 birds left, then 3 birds right, and finally 1 bird left...
--  ...but sadly the code reads backwards


--
-- Throwing Pierre off balance with the Maybe Monad
--

--
-- landLeft': Take a number of Birds and land them on the Left side of the Pole (if that's possible)
--            To make Birds fly away just enter a negative number of Birds
--
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing
-- landLeft' 3 (0,0) --> Just (3,0)
-- landLeft' 5 (0,0) --> Nothing

--
-- landRight': Take a number of Birds and land them on the Right side of the Pole (if that's possible)
--             To make Birds fly away just enter a negative number of Birds
--
landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing
-- landRight' 3 (0,0) --> Just (0,3)
-- landRight' 5 (0,0) --> Nothing

