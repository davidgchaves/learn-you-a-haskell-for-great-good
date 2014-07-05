-- Syntax in functions

--
-- Pattern Matching
--

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

-- this is brilliant!!!
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


--
-- Pattern Matching with tuples
--

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z


--
-- Pattern Matching with lists and lists comprehensions
--

xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
patternMatchingInListComprehensions = [x+y | (x,y) <- xs]

head' :: [a] -> a
head' [] = error "Can't call head' on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y


--
-- Pattern matching and as-patterns
--

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- all@(x:xs) is an example of an as-pattern
-- you could use whatever name you like instead 'all', i.e. sentence@(x:xs)


--
-- Guards
--

bmiTell :: Double -> Double -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b    = EQ
    | a <= b    = LT
    | otherwise = GT


--
-- Where keyword
--

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0


--
-- Pattern Matching with where
--

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
-- just an example of pattern matching with where,
-- but pattern matching directly in the function's parameters
-- seems a better choice:
initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."


--
-- Functions in where blocks
--

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2


--
-- Let expressions
--

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

--
-- Let in list comprehensions
--

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


--
-- Case expressions
--

head'' :: [a] -> a
head'' xs = case xs of []    -> error "No head for empty lists!"
                       (x:_) -> x
-- Equivalent to head' (pattern matching with lists and list comprehensions)

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a longer list."

