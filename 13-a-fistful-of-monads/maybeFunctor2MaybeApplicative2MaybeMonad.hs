--
-- A value of a 'Maybe a' represents a value of 'type a'
-- but with the context of possible failure attached
--

--
-- Maybe as a Functor:
--

--  If we want to fmap a function over a 'Maybe a'
--      - if it's a 'Just a' value -> the function is mapped over what's inside (the 'a')
--      - if it's a Nothing value  -> the Nothing is kept, because there's nothing to map it over
--
-- fmap (++ "!") $ Just "wisdom" --> Just "wisdom!"
-- fmap (++ "!") Nothing         --> Nothing


--
-- Maybe as an Applicative Functor:
--

--  Now, the function itself is in a context too (along with the 'Maybe a' value)
--  If we want to use <*> to apply a function inside a Maybe over a 'Maybe a'
--      - if both are 'Just a' values -> The function inside the Maybe is applied over
--                                       what's inside the 'Maybe a' value
--      - if there's a Nothing        -> The Nothing is kept
--
-- NOTE: Remember to import the Control.Applicative module
-- Just (+ 3) <*> Just 4 --> Just 7
--      - (+ 3) is the function
--      - Just (+ 3) is the function wrapped in a Maybe
--      - Just 4 is the 'Maybe a' value
-- Just (+ 3) <*> Nothing --> Nothing
--      - (+ 3) is the function
--      - Just (+ 3) is the function wrapped in a Maybe
--      - Nothing is the 'Maybe a' value
-- Nothing <*> Just 4 --> Nothing
--      - Nothing is the function wrapped in a Maybe (Errr... well...)
--      - Just 4 is the 'Maybe a' value

-- Using the Applicative Style:
-- (+) <$> Just 3 <*> Just 4  --> Just 7
-- (+) <$> Just 3 <*> Nothing --> Nothing
-- (+) <$> Nothing <*> Just 4 --> Nothing


--
-- Maybe as a Monad
--

-- We'd like to take a 'Maybe a' value and a fuunction of type 'a -> Maybe b'
-- and apply that function to the 'Maybe a' value
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x
-- Just 3 `applyMaybe` (\x -> Just (x + 1))  --> Just 4
-- Nothing `applyMaybe` (\x -> Just (x + 1)) --> Nothing

-- Just 3 `applyMaybe` (\x -> if x > 2 then Just (x + 1) else Nothing) --> Just 4
-- Just 1 `applyMaybe` (\x -> if x > 2 then Just (x + 1) else Nothing) --> Nothing

