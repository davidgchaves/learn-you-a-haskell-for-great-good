--
-- Moar Functors!!!
--

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b


--
-- IO as an instance of the Functor type class
--

-- fmap' :: (a -> b) -> f a  -> f b
-- fmap' :: (a -> b) -> IO a -> IO b
instance Functor' IO where
    fmap' f action = do
        result <- action
        return (f result)
-- When we fmap a function f over an I/O action,
-- we want to get back an I/O action that does the same thing
-- but has our function f applied over its result value

