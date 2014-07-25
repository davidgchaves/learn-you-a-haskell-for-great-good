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


--
-- Function as an instance of the Functor type class (Control.Monad.Instances)
-- AKA Function Composition
--

-- fmap' :: (a -> b) -> f a      -> f b
-- fmap' :: (a -> b) -> (r -> a) -> (r -> b) -- (comes from) ((->) r a) -> ((->) r b)

--instance Functor' ((->) r) where
--    fmap' f g = (\x -> f (g x))

-- Mapping  a function f :: a -> b over a function g :: r -> a
-- produces a function h :: r -> b ...and that's Function Composition in a nutshell:
-- h(x) = fog(x) = f(g(x))

-- Alternative way to implement this making clear that
-- using fmap over functions is just Function Composition
instance Functor' ((->) r) where
    fmap' = (.)
-- :t fmap' (*3) (+100)    --> fmap' (*3) (+100) :: Num a => a -> a
-- fmap' (*3) (+100) 1     --> 303
-- (*3) `fmap'` (+100) $ 1 --> 303
-- (*3) . (+100) $ 1       --> 303

