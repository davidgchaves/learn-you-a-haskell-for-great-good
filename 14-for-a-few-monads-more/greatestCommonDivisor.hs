import Control.Monad.Writer

--
-- gcd': Euclid's algorithm
--
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0    = a
    | otherwise = gcd' b (a `mod`b)
-- gcd' 12 5 --> 1

--
-- gcd'': Euclid's algorithm with logging using the Writer Monad
--
gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
    | b == 0    = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd'' b (a `mod` b)
-- fst $ runWriter (gcd'' 12 5) --> 1
-- mapM_ putStrLn $ snd $ runWriter (gcd'' 12 5)
--  --> 12 mod 5 = 2
--  --> 5 mod 2 = 1
--  --> 2 mod 1 = 0
--  --> Finished with 1
