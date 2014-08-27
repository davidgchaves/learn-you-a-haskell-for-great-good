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


--
-- addDrink: Adds drink to some cowboy food order
--

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)
-- ("beans", Sum 10) `applyLog` addDrink                      --> ("milk", Sum {getSum = 35})
-- ("jerky", Sum 25) `applyLog` addDrink                      --> ("whiskey", Sum {getSum = 124})
-- ("dogmeat", Sum 5) `applyLog` addDrink                     --> ("beer", Sum {getSum = 35})
-- ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink --> ("beer", Sum {getSum = 65})

