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

