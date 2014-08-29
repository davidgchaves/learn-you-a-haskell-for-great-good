import Control.Monad.Writer

--
-- logNumber: produce a Writer value out of a number
--
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])
-- runWriter $ logNumber 4 --> (4, ["Got number: 4"])

--
-- multWithLog: using the Writer Monad with 'do notation'
--
multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 5
    b <- logNumber 10
    tell ["Gonna multiply those two"]
    return (a*b)
-- runWriter multWithLog --> (50, ["Got number: 5","Got number: 10","Gonna multiply those two"])

