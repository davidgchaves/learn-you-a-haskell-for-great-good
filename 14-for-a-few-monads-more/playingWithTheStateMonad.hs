import Control.Monad.State

--
-- Modeling a Stack
--
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

--
-- stackManip: take a stack
--             push 3 to it
--             pop 2 items
--  NOTE: The pushed 3 gets lost when you look from the outside
--
stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a , newStack2) = pop newStack1
    in pop newStack2
-- stackManip [5,8,2,1] --> (5, [8,2,1])


--
-- Modeling a Stack with the State Monad
--

pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a:xs)

