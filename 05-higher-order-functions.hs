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


mapSquareWithMap :: (Num a) => [[a]] -> [[a]]
mapSquareWithMap = map (map (^ 2))
-- mapSquareWithMap [[1,2,3], [10,20,30]] --> [[1,4,9],[100,400,900]]

mapSquareWithListComprehensions :: (Num a) => [[a]] -> [[a]]
mapSquareWithListComprehensions xs = [map (^ 2) x | x <- xs]
-- mapSquareWithListComprehensions [[1,2,3], [10,20,30]] --> [[1,4,9],[100,400,900]]


firstWithMap :: [(a,b)] -> [a]
firstWithMap = map fst
-- firstWithMap [(1,"What"), (2,"Where"), (3,"Who")] --> [1,2,3]

firstWithListComprehensions :: [(a,b)] -> [a]
firstWithListComprehensions xs = [fst x | x <- xs]
-- firstWithListComprehensions [(1,"What"), (2,"Where"), (3,"Who")] --> [1,2,3]


--
-- The filter' function
--

-- takes a predicate function (a -> Bool) and a list [a]
-- produces a list [a] with the elements that satisfy the predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs


--
-- filter vs list comprehensions with predicates
--

greaterThan3WithFilter :: [Int] -> [Int]
greaterThan3WithFilter = filter (> 3)
-- greaterThan3WithFilter [1,2,3,4,5] --> [4,5]

greaterThan3WithListComprehensions :: [Int] -> [Int]
greaterThan3WithListComprehensions xs = [x | x <- xs, x > 3]
-- greaterThan3WithListComprehensions [1,2,3,4,5] --> [4,5]


onlyEvenWithFilter :: [Int] -> [Int]
onlyEvenWithFilter = filter even
-- onlyEvenWithFilter [1..10] --> [2,4,6,8,10]

onlyEvenWithListComprehensions :: [Int] -> [Int]
onlyEvenWithListComprehensions xs = [x | x <- xs, even x]
-- onlyEvenWithListComprehensions [1..10] --> [2,4,6,8,10]


notNullWithFilter :: [[a]] -> [[a]]
notNullWithFilter = let notNull x = not (null x) in filter notNull
-- A bit convoluted.
-- I think this time explicitly requiring xs as an arg helps readability

notNullWithFilter' :: [[a]] -> [[a]]
notNullWithFilter' xs = let notNull x = not (null x) in filter notNull xs
-- I think this reads better. Not guessing about partially applying filter.

-- notNullWithFilter  [[1,2,3], [], [4], [5,6], [], [], []] --> [[1,2,3],[4],[5,6]]
-- notNullWithFilter' [[1,2,3], [], [4], [5,6], [], [], []] --> [[1,2,3],[4],[5,6]]

notNullWithListComprehensions :: [[a]] -> [[a]]
notNullWithListComprehensions xs = [x | x <- xs, let notNull x = not (null x), notNull x]
-- notNullWithListComprehensions [[1,2,3], [], [4], [5,6], [], [], []] --> [[1,2,3],[4],[5,6]]


capitalLettersOnlyWithFilter :: String -> String
capitalLettersOnlyWithFilter = filter (`elem` ['A'..'Z'])
-- capitalLettersOnlyWithFilter "i LAuGh at you bEcause u R all the same" --> "LAGER"

capitalLettersOnlyWithListComprehensions :: String -> String
capitalLettersOnlyWithListComprehensions xs = [x | x <- xs, x `elem` ['A'..'Z']]
-- capitalLettersOnlyWithListComprehensions "i LAuGh at you bEcause u R all the same" --> "LAGER"


lessThan15AndEvenWithFilter :: Integral a => [a] -> [a]
lessThan15AndEvenWithFilter xs = filter (< 15) (filter even xs)
-- lessThan15AndEvenWithFilter [1..20] --> [2,4,6,8,10,12,14]

lessThan15AndEvenWithListComprehensions :: Integral a => [a] -> [a]
lessThan15AndEvenWithListComprehensions xs = [x | x <- xs, x < 15, even x]
-- lessThan15AndEvenWithListComprehensions [1..20] --> [2,4,6,8,10,12,14]


quicksortWithFilter :: (Ord a) => [a] -> [a]
quicksortWithFilter [] = []
quicksortWithFilter (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger         = filter (> x) xs
    in  quicksortWithFilter smallerOrEqual ++ [x] ++ quicksortWithFilter larger
-- quicksortWithFilter "the quick brown fox jumps over the lazy dog"
--                     --> "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"

quicksortWithListComprehensions :: (Ord a) => [a] -> [a]
quicksortWithListComprehensions [] = []
quicksortWithListComprehensions (x:xs) =
    let smallerOrEqual = [e | e <- xs, e <= x]
        larger         = [e | e <- xs, e > x]
    in  quicksortWithListComprehensions smallerOrEqual ++ [x] ++ quicksortWithListComprehensions larger
-- quicksortWithListComprehensions "the quick brown fox jumps over the lazy dog"
--                                 --> "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"


--
-- More examples of map and filter
--

largestDivisibleUnder100000 :: Integer -> Integer
largestDivisibleUnder100000 x = head (filter pred [100000, 99999..])
    where pred y = y `mod` x == 0
-- largestDivisibleUnder100000 3829 --> 99554

largestDivisibleUnder100000' :: Integer -> Integer
largestDivisibleUnder100000' x =
    let pred y = y `mod` x == 0
    in  head (filter pred [100000, 99999..])
-- largestDivisibleUnder100000' 3829 --> 99554


sumOddSquaresUnder :: Integer -> Integer
sumOddSquaresUnder num = sum (takeWhile (< num) (filter odd (map (^ 2) [1..])))
-- sumOddSquaresUnder 10000 --> 166650

produceSquares :: [Integer]
produceSquares = map (^ 2) [1..]

filterOddSquares :: [Integer]
filterOddSquares = filter odd produceSquares

oddSquaresUnder :: Integer -> [Integer]
oddSquaresUnder num = takeWhile (< num) filterOddSquares

sumOddSquaresUnder' :: Integer -> Integer
sumOddSquaresUnder' num = sum (oddSquaresUnder num)
-- sumOddSquaresUnder' 10000 --> 166650
-- I like the 2nd implementation the better


produceCollatzChain :: Integer -> [Integer]
produceCollatzChain 1 = [1]
produceCollatzChain n
    | even n = n : produceCollatzChain (n `div` 2)
    | odd n  = n : produceCollatzChain (n * 3 + 1)

produceFirst100CollatzChains :: [[Integer]]
produceFirst100CollatzChains = map produceCollatzChain [1..100]

filterCollatzChainsLongerThan15 :: [[Integer]]
filterCollatzChainsLongerThan15 = filter isLong produceFirst100CollatzChains
    where isLong xs = length xs > 15

countLongCollatzChains :: Int
countLongCollatzChains = length filterCollatzChainsLongerThan15
-- countLongCollatzChains --> 66


--
-- Fold
--

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- the binary function (lambda) in a foldl takes,
-- first the acc and second the current list value

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0
-- we can omit the xs parameter because foldl (+) 0
-- will return a function that takes a list (because of currying)


elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys
-- the binary function (lambda) in foldr takes,
-- first the current list value and second the acc


--
-- using foldr to implement map
--

mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs
-- fast because of :


--
-- using foldl to implement map
--

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs
-- slow because of ++


--
-- using foldl to implement reverse
--

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []


--
-- using foldl to implement product
--

product' :: (Num a) => [a] -> a
product' = foldl (*) 1


--
-- using foldr to implement filter
--

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' pred = foldr (\x acc -> if pred x then x : acc else acc) []


--
-- using foldl1 to implement last
--

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
--the last element will be returned by the lambda once the list is fully traversed


--
-- using foldr on infinite lists: and
--

-- foldr will work on infinite lists when the binary function that we're passing to it
-- doesn't always need to evaluate its second parameter to give us some sort of answer
and' :: [Bool] -> Bool
and' = foldr (&&) True
-- and' (repeat False)           --> False
-- and' (take 100 (repeat True)) --> True


--
-- Function composition
--


--
-- lambda vs function composition: turnIntoNegative
--

turnIntoNegative :: [Integer] -> [Integer]
turnIntoNegative = map (\x -> negate (abs x))
-- turnIntoNegative [(-4)..3] --> [-4,-3,-2,-1,0,-1,-2,-3]

turnIntoNegative' :: [Integer] -> [Integer]
turnIntoNegative' = map (negate . abs)
-- turnIntoNegative' [(-4)..3] --> [-4,-3,-2,-1,0,-1,-2,-3]


--
-- lambda vs function composition: negateTheSumOfTheTails
--

negateTheSumOfTheTails :: [[Integer]] -> [Integer]
negateTheSumOfTheTails = map (\xs -> negate(sum(tail xs)))
-- negateTheSumOfTheTails [[1..5], [3..6], [1..7]] --> [-14,-15,-27]

negateTheSumOfTheTails' :: [[Integer]] -> [Integer]
negateTheSumOfTheTails' = map (negate . sum . tail)
-- negateTheSumOfTheTails' [[1..5], [3..6], [1..7]] --> [-14,-15,-27]


--
-- point-free style example
--

fn :: (Floating a, Integral b, RealFrac a) => a -> b
fn x = ceiling (negate (tan (cos (max 50 x))))
-- fn 180 --> 1

pointFreeStyleFn :: (Floating a, Integral b, RealFrac a) => a -> b
pointFreeStyleFn = ceiling . negate . tan . cos . max 50
-- pointFreeStyleFn 180 --> 1

