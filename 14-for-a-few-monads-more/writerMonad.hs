--
-- The Writer Monad
--

-- The Writer Monad allows us to do computations while making sure that
-- all the log values are combined into one log value, which then is attached to the result


--
-- isBigGang: takes a number of bandits in a gang and tells us
--            if that's a big gang and a description
--
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")
-- isBigGang 5  --> (False, "Compared gang size to 9.")
-- isBigGang 10 --> (True, "Compared gang size to 9.")
--
-- We have a value (True or False) and some context (a description)

