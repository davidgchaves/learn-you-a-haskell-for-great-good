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

