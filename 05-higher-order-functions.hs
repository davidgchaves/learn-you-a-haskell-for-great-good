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


--
-- Higher-order function's examples
--

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- we need the (a -> a) because -> is naturally right-associative

sum3 :: Int -> Int
sum3 = (+ 3)
-- applyTwice sum3 10 -> 16

postHaHa :: String -> String
postHaHa = (++ " HAHA")
-- applyTwice postHaHa "HEY" --> "HEY HAHA HAHA"

preHaHa :: String -> String
preHaHa = ("HAHA " ++)
-- applyTwice preHaHa "HEY" --> "HAHA HAHA HEY"

cons3 :: [Int] -> [Int]
cons3 = (3 :)
-- applyTwice cons3 [1] --> [3, 3, 1]


--
-- Implementing zipWith'
--

-- f takes two args (a and b) and returns one value (c)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' (*) [10, 100, 1000] [4, 5, 6] --> [40, 500, 6000]


--
-- Implementing flip' (... and my head exploded)
--

-- f takes two args (a and b) and returns one value (c)
-- g takes two args (b and a) and returns one value (c)
-- flip' takes the function f and returns the function g
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f x y = f y x


--
-- The map' function
--

map' :: (a -> b) -> [a] -> [b]
map' _ []      = []
map' f (x:xs) = f x : map' f xs


--
-- map vs list comprehensions
--

plus3WithMap :: [Int] -> [Int]
plus3WithMap = map (+ 3)
-- plus3WithMap [1,2,3,4,5,6,7,8,9] --> [4,5,6,7,8,9,10,11,12]

plus3WithListComprehensions :: [Int] -> [Int]
plus3WithListComprehensions xs = [x + 3 | x <- xs]
-- plus3WithListComprehensions [1,2,3,4,5,6,7,8,9] --> [4,5,6,7,8,9,10,11,12]


bangWithMap :: [String] -> [String]
bangWithMap = map (++ "!")
-- bangWithMap ["ha", "he", "hi"] --> ["ha!","he!","hi!"]

bangWithListComprehensions :: [String] -> [String]
bangWithListComprehensions xs = [x ++ "!" | x <- xs]
-- bangWithListComprehensions ["ha", "he", "hi"] --> ["ha!","he!","hi!"]


replicate3WithMap :: [a] -> [[a]]
replicate3WithMap = map (replicate 3)
-- replicate3WithMap ["Hello", "World"] --> [["Hello","Hello","Hello"],["World","World","World"]]

replicate3WithListComprehensions :: [a] -> [[a]]
replicate3WithListComprehensions xs = [replicate 3 x | x <- xs]
-- replicate3WithListComprehensions ["Hello", "World"] --> [["Hello","Hello","Hello"],["World","World","World"]]

