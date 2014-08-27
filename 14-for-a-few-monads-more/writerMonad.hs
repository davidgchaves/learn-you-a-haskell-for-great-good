import Data.Monoid

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


--
-- applyLog: to a value and an accompanying monoid value
--

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)
-- (3, "Smallish gang.") `applyLog` isBigGang       --> (False, "Smallish gang.Compared gang size to 9.")
-- (30, "A freaking platoon.") `applyLog` isBigGang --> (True, "A freaking platoon.Compared gang size to 9.")
-- ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
--      --> (5, "Got outlaw name.Applied length.")

