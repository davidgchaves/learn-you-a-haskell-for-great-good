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


--
-- The recursive take' function
--

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ []         = []
take' n (x:xs)     = x : take' (n-1) xs


--
-- The recursive reverse' function
--

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


--
-- The recursive repeat' function
--

repeat' :: a -> [a]
repeat' x = x : repeat' x
-- nice example of how to use recursion with no base case to produce infinite lists


--
-- The recursive zip' function
--

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

