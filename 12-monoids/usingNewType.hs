--
-- Enter the Pair newtype to make Functors out of Tuples
--

newtype Pair b a = Pair { getPair :: (a,b) }

-- fmap :: (a -> b) -> f a      -> f b
-- fmap :: (a -> b) -> Pair c a -> Pair c b
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)
-- getPair $ fmap (*100) (Pair (1,5)) --> (100,5)


--
-- data (ADT) not-so-good lazyness vs newtype lazyness
--

-- ADT TIME!
-- CoolBool is an ordinary Algebraic Data Type
data CoolBool = CoolBool { getCoolBool :: Bool }

-- helloMe just pattern matches on a CoolBool
--         and returns "hello" regardless CoolBool is True or False
--         ...with a twist...
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
-- helloBool works as expected:
-- helloMe $ CoolBool True  --> "hello"
-- helloMe $ CoolBool False --> "hello"
-- ...but her comes the twist:
-- helloMe undefined        --> "*** Exception: Prelude.undefined
--
-- WHY?
-- Types defined with the data keyword can have multiple value constructors (even though CoolBool has only one).
-- So in order to see if the value given to our function conforms to the (CoolBool _) pattern,
-- Haskell must evaluate the value just enough to see which value constructor was used when we made the value.
-- And when we try to evaluate an undefined value, even a little, an exception is thrown.

-- NEWTYPE TIME!
-- CoolBool' is a newtype
newtype CoolBool' = CoolBool' { getCoolBool' :: Bool }

-- helloMe' just pattern matches on a CoolBool'
--          and returns "hello" regardless CoolBool' is True or False
--          ...with NO twist...
helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"
-- helloMe' $ CoolBool' True  --> "hello"
-- helloMe' $ CoolBool' False --> "hello"
-- helloMe' undefined         --> "hello"
--
-- WHY?
-- When we use newtype, Haskell can internally represent the values of the new type in the same way as the original values.
-- It doesn't need to add another box around them; it just must be aware of the values being of different types.
-- And because Haskell knows that types made with the newtype keyword can have only one constructor,
-- it doesn't need to evaluate the value passed to the function to make sure that the value conforms to the (CoolBool _) pattern,
-- because newtype types can have only one possible value constructor and one field!

