--
-- The Writer Monad
--

-- The Writer Monad allows us to do computations while making sure that
-- all the log values are combined into one log value, which then is attached to the result


--
-- isBigGang: takes a number of bandits in a gang and tells us if that's a big gang
--
isBigGang :: Int -> Bool
isBigGang x = x > 9

