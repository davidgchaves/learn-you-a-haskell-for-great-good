--
-- liftM: Even though every Monad is a Functor, we don't need to rely on it
--        having a Functor instance because of the liftM function.
--        liftM is implemented without referencing the Functor type class at all.
--
liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f m = do
    x <- m
    return (f x)

-- liftM' (*3) (Just 8) --> Just 24

