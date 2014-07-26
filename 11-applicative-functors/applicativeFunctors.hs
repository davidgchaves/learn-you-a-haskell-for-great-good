--
-- The Applicative type class as defined in the Control.Applicative module
--

class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-- class (Functor f) => Applicative f means:
--  If we want to make a type constructor part of the Applicative type class,
--  it must be in Functor first.
--  So if we know that a type constructor is part of the Applicative type class,
--  it's also in Functor (so we can use fmap on it).
--
-- pure :: a -> f a means that:
--  It takes a value and puts it in some sort of default or pure context;
--  a minimal context that still yields that value.
--
-- (<*>) :: f (a -> b) -> f a -> f b means that:
--  It takes two functor values, the first one with a function inside,
--  and extracts the function from the first functor value,
--  and maps it over the second functor value.


--
-- The Applicative instance implementation of Maybe
--

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
-- Just (++"hahaha") <*> Nothing --> Nothing
-- Nothing <*> Just "woot"       --> Nothing
-- Just (+3) <*> Just 9          --> Just 12
-- pure (+3) <*> Just 9          --> Just 12
--  Try to use pure (instead of Just) in an applicative context (using <*>)


--
-- The Applicative instance implementation of [] (the list type constructor)
--

-- (<*>) :: f (a -> b)  -> f a  -> f b
-- (<*>) :: [a -> b]    -> [a]  -> [b]
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
-- pure "Hey" :: [String]          --> ["Hey"]
-- pure "Hey" :: Maybe String      --> Just "Hey"
-- [(*0),(+100),(^2)] <*> [1,2,3]  --> [0,0,0, 101,102,103, 1,4,9]
-- [(+),(*)] <*> [1,2] <*> [10,20] --> [11,21,12,22, 10,20,20,40]


--
-- The Applicative instance implementation of IO
--

-- (<*>) :: f (a -> b)  -> f a  -> f b
-- (<*>) :: IO (a -> b) -> IO a -> IO b
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)

