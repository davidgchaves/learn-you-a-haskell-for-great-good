-- Hello recursion!

--
-- The recursive maximum' function
--

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "maximum of empty list!"
maximum' (x:[]) = x
maximum' (x:xs) = max x (maximum' xs)
-- "maximum' (x:[])" could be expressed as "maximum' [x]" too
-- I don't know why but I like the explicit pattern matching better


--
-- The recursive replicate' function
--

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- In the book guards are used, instead of pattern matching:
replicate'' :: Int -> a -> [a]
replicate'' n x
    | n <= 0    = []
    | otherwise = x : replicate'' (n-1) x
-- and there's a good reason: Protecting against n being negative

