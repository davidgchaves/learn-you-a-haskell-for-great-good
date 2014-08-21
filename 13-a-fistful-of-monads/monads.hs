import Control.Applicative
--
-- The Monad Type Class
--

-- Every Monad is an Applicative Functor
class (Applicative m) => Monad' m where

    -- 'return' takes a value and puts it in a minimal default context
    -- that still holds the value (wraps a value in a Monad)
    return' :: a -> m a

    -- '>>=' or 'bind'
    (>>>=) :: m a -> (a -> m b) -> m b

    (>>>) :: m a -> m b -> m b
    x >>> y = x >>>= (\_ -> y)

    fail' :: String -> m a
    fail' msg = error msg


--
-- Maybe is an instance of Monad
--

instance Monad' Maybe where
    return' x = Just x

    Nothing >>>= f = Nothing
    Just x >>>= f = f x

    fail' _ = Nothing
-- return' "WHAT" :: Maybe String     --> Just "WHAT"
-- Just 5 >>>= (\x -> Just (x * 10))  --> Just 50
-- Nothing >>>= (\x -> Just (x * 10)) --> Nothing


--
-- A Monad instance for lists
--

instance Monad' [] where
    return' x = [x]

    xs >>>= f = concat (map f xs)

    fail' _ = []
-- return' 4 :: [Int]                          --> [4]
-- [3,4,5] >>>= \x -> [x,-x]                   --> [3,-3, 4,-4, 5,-5]
-- [] >>>= \x -> ["failing", "like", "a boss"] --> []


--
-- MonadPlus: Monads that can also act as Monoids
--

class (Monad' m) => MonadPlus' m where
    -- mzero is synonymous with mempty
    mzero' :: m a

    -- mplus is synonymous with mappend
    mplus' :: m a -> m a -> m a

