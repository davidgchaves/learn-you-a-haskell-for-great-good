import Control.Monad.Writer

--
-- logNumber: produce a Writer value out of a number
--
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])
-- runWriter $ logNumber 4 --> (4, ["Got number: 4"])

