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

--
-- stackManip': Simplifying stackManip thanks to the State Monad
--
stackManip' :: State Stack Int
stackManip' = do
    push' 3
    pop'
    pop'
-- runState stackManip' [5,8,2,1] --> (5, [8,2,1])

--
-- stackStuff: take a stack
--             pop 1 item from the stack
--                  if it's 5       push 5 back
--                  if it's not 5   push 3 and 8
--
stackStuff :: State Stack ()
stackStuff = do
    a <- pop'
    if a == 5
        then push' 5
        else do
            push' 3
            push' 8
-- runState stackStuff [9,0,2,1,0] --> ((), [8,3,0,2,1,0])
-- runState stackStuff [5,0,2,1,0] --> ((), [5,0,2,1,0])

