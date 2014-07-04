-- Baby's first functions

doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

-- if is an expression (must return a value), not an statement
doubleSmallNumber x = if x > 100 then x else x * 2

-- ' usually denotes a strict or slightly modified version
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1


--
-- list comprehensions => A way to filter, transform and combine lists
--

doubleTheInput = [x*2 | x <- [1..10]] -- --> [2,4,6,8,10,12,14,16,18,20]
-- x <- [1..10]  --> bind each element from 1 to 10 to x.
-- x*2           --> DRAW: double each element.

-- NOTE TO SELF: So this is where Scala's list comprehensions came from :)

-- adding a predicate to a list comprehension
doubleTheInputAndFilter = [x*2 | x <- [1..10], x*2 >= 12] -- --> [12,14,16,18,20]
-- x*2 >= 12            --> filter (only output) elements which satisfy the predicate

-- numbers from 50 to 100 whose remainder when divided by 7 is 3
filterAccordingToRemainder = [x | x <- [50..100], x `mod` 7 == 3] -- --> [52,59,66,73,80,87,94]


-- list comprehension that replaces every odd number less than 10 with "BOOM!"
--                              and every odd number greater than 10 with "BANG!"
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
-- the 'odd number filtering' is done in the predicate
-- the binding is dependent on the xs argument
-- DRAW: the output is responsible for the actual replacement

usingMoreThanOnePredicateAtOnce = [x | x <- [10..20], x/=13, x/= 15, x/= 17] -- --> [10,11,12,14,16,18,19,20]

drawValuesFromTwoLists = [x+y | x <- [1,2,3], y <- [10, 100, 1000]] -- --> [11,101,1001,12,102,1002,13,103,1003]

stringsAreListsTooSoRemoveNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],
       [1,2,3,4,5,6,7,8,9],
       [1,2,4,2,1,6,3,1,3,2,3,6]]
nestedListComprehensionOperatingOnNestedLists = [ [x | x <- xs, even x] | xs <- xxs] -- --> [[2,2,4], [2,4,6,8], [2,4,2,6,2,6]]


--
-- tuples => Store several heterogeneous elements as a single value
--

aPair = (1, 3)
aTriple = (3, 'a', "hello")
aFourTuple = (50, 50.4, "hello", 'b')
-- a pair, a triple, a 4-tuple, ... are DISTINCT types (i.e. a list CAN'T be composed of both pairs and triples)
-- tuples that have the same length but have different types of data are DISTINCT types of tuples

returnThe1stComponentOfATuple = fst (8, 11) -- --> 8
returnThe2ndComponentOfATuple = snd (8, 11) -- --> 11

returnAListOfPairs = zip [1..5] ["one", "two", "three", "four", "five"] -- --> [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]

