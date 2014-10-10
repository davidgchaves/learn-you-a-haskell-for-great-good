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

--
-- moreStack: Gluing stackManip' and stackStuff together
--
moreStack :: State Stack ()
moreStack = do
    a <- stackManip'
    if a == 100
        then stackStuff
        else return ()
-- runState moreStack [9,  0,2,1,0] --> ((), [0,2,1,0])
-- runState moreStack [100,0,2,1,0] --> ((), [8,3,2,1,0])


--
-- stackyStack: using 'get' and 'put' from the MonadState type class
--
stackyStack :: State Stack ()
stackyStack = do
    currentStack <- get
    if currentStack == [1,2,3]
        then put [5,5,5]
        else put [8,8,8]
-- runState stackyStack [9,0,2] --> ((), [8,8,8])
-- runState stackyStack [1,2,3] --> ((), [5,5,5])

--
-- pop in terms of 'get' and 'put'
--
pop'' :: State Stack Int
pop'' = do
    (x:xs) <- get
    put xs
    return x
-- runState pop'' [1,2,3,4,5] --> (1, [2,3,4,5])

--
-- push in terms of 'get' and 'put'
--
push'' :: Int -> State Stack ()
push'' x = do
    xs <- get
    put (x:xs)
-- runState (push'' 0) [1,2,3,4,5] --> ((), [0,1,2,3,4,5])

