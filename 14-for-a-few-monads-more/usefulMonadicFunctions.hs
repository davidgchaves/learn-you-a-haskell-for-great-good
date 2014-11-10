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


--
-- join: Takes a monadic value within a monadic value
--       and gives us just a monadic value (it flattens it)
--
join' :: (Monad m) => m (m a) -> m a
join' mm = do
    m <- mm
    m
-- join' (Just (Just 9))                        --> Just 9
-- join' [[1,2,3], [4,5,6]]                     --> [1,2,3,4,5,6]
-- join' (Right (Right 9)) :: Either String Int --> Right 9


--
-- filterM: generalizes the list-based 'filter' function
--  * Takes
--      - a monadic predicate: returns a monadic value whose result is a Bool
--      - a list to filter: plain old list
--  * Returns
--      - a monadic value: whose result is a plain old list
--
filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' _ []     = return []
filterM' p (x:xs) = do
    flag <- p x
    ys <- filterM' p xs
    return (if flag then x:ys else ys)

