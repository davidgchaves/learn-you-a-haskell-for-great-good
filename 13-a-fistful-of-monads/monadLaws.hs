--
-- 1st Monad Law: LEFT IDENTITY
--

-- This is how "I" get this:
--  A value 'x' and a value with a minimal context (monadic value) 'return x'
--  behave the same when feeding them into a function 'f':
--      Feed the value 'x' to a function 'f'                => f x
--      Feed the monadic value 'return x' to a funcion 'f'  => return x >>= f
--
--  Hence: 'return x >>= f' is the same as 'f x'

-- For the Maybe Monad => return is defined as Just
-- return 3 >>= (\x -> Just $ x + 100)  --> Just 103
-- (x -> Just $ x + 100) 3              --> Just 103

-- For the List Monad => return puts something in a singleton list
-- return "WoW" >>= (\x -> [x,x,x]) --> ["WoW","WoW","WoW"]
-- (\x -> [x,x,x]) "WoW"            --> ["WoW","WoW","WoW"]


--
-- 2nd Monad Law: RIGHT IDENTITY
--

-- When we feed the monadic value 'm' to the function 'return',
-- the result is the monadic value 'm'

-- 'm >>= return' is the same as 'm'

-- For the Maybe Monad it means that 'return' doesn't introduce any failure
-- Just "move on up" >>= (\x -> return x) --> Just "move on up"

-- For the List Monad it means that 'return' doesn't introduce any extra nondeterminism
-- [1,2,3,4] >>= (\x -> return x) --> [1,2,3,4]


--
-- 1st and 2nd Monad Laws: It's all about how 'return' should behave
--

-- 'return' makes normal values into monadic ones and
-- the produced monadic value shouldn't have any more than the minimal context needed


--
-- 3rd Monad Law: ASSOCIATIVITY
--

-- It doesn't matter how you nest feeding values to monadic function:
--  feeding the monadic value 'm' to the monadic function 'f' and then to the monadic function 'g'
--  is the same as
--  feeding the monadic value 'm' to a function that feeds the result of 'f x' to 'g'

-- '(m >>= f) >>= g' is the same as 'm >>= (\x -> f x >>= g)'

