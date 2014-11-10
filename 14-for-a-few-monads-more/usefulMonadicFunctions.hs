import Control.Monad.Writer -- for the filterM example
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

-- filterM Examples:
--  (1) using the lessThanFour monadic predicate
lessThanFour :: Int -> Writer [String] Bool
lessThanFour x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False
-- fst $ runWriter $ filterM' lessThanFour [9,1,5,2,10,3] --> [1,2,3]
-- snd $ runWriter $ filterM' lessThanFour [9,1,5,2,10,3] -->
--  ["9 is too large, throwing it away",
--   "Keeping 1",
--   "5 is too large, throwing it away",
--   "Keeping 2",
--   "10 is too large, throwing it away",
--   "Keeping 3"]

