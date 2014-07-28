--
-- Enter the Pair newtype to make Functors out of Tuples
--

newtype Pair b a = Pair { getPair :: (a,b) }

-- fmap :: (a -> b) -> f a      -> f b
-- fmap :: (a -> b) -> Pair c a -> Pair c b
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)
-- getPair $ fmap (*100) (Pair (1,5)) --> (100,5)

