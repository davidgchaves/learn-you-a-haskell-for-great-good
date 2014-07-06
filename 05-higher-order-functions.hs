-- Higher-order functions

--
-- Curried and partially applied functions
--

-- Concepts:
--   curried function: function that takes only one parameter (instead of taking several)
--   partially applied function: function returned when we call a function with too few parameters

-- max is a 'curried function' under the hood
-- (takes 1 parameter, returns a function that takes the next parameter, ...)
-- Example: partialMax3 leverages that power using a 'partially applied' max
partialMax3 :: Int -> Int
partialMax3 x = max 3 x
-- partialMax3 4 --> 4
-- partialMax3 2 --> 2

-- Example: partialMax3' is 100% equivalent to partialMax3
partialMax3' :: Int -> Int
partialMax3' = max 3
-- partialMax3' 4 --> 4
-- partialMax3' 2 --> 2

-- Example: compareWith100 captures a 'partially applied' compare
compareWith100 :: Int -> Ordering
compareWith100 = compare 100
-- compareWith100 99  --> GT
-- compareWith100 100 --> EQ
-- compareWith100 101 --> LT

-- Example: 'partially applying' / infix function by using 'sections'
divideBy10 :: (Floating a) => a -> a
divideBy10 = (/ 10)
-- divideBy10 100 --> 10.0

-- Example: 'partially applying' elem infix function by using 'sections'
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
-- isUpperAlphanum 'g' --> False
-- isUpperAlphanum 'G' --> False

