-- Modules

--
-- Importing modules
--

--
-- import zone
--
import Data.List
import Data.Char
import qualified Data.Map as Map
--
--
--

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- numUniques [1,2,3,4,5,1,2,3,5] --> 5


--
-- Counting words
--

numWords :: String -> [(String, Int)]
numWords = map (\xs -> (head xs, length xs)) . groupWords
    where groupWords = group . sort . words
-- numWords "hodor dhodr hodor hodoir hodor" --> [("dhodr",1),("hodoir",1),("hodor",3)]


--
-- Needle in the haystack (isInfixOf)
--

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
-- [1,4] `isIn` [1,2,3,4,5] --> False
-- [3,4] `isIn` [1,2,3,4,5] --> True


--
-- Caesar cipher salad
--

-- It needs 'import Data.Char', but haskell is a bit picky and
-- it seems it needs all imports at the beginning of the file

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg
-- encode 6 "Hey Mark, instruct your men to party hard!"
--          --> "Nk\DEL&Sgxq2&otyzx{iz&\DELu{x&skt&zu&vgxz\DEL&ngxj'"

decode :: Int -> String -> String
decode offset msg = map (\c -> chr $ ord c - offset) msg
-- decode 6 "Nk\DEL&Sgxq2&otyzx{iz&\DELu{x&skt&zu&vgxz\DEL&ngxj'"
--          --> "Hey Mark, instruct your men to party hard!"


--
-- Let's find some cool numbers and return a Maybe Int :)
--

sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show
-- sumDigits 2345 --> 14

firstToSum :: Int -> Maybe Int
firstToSum n = find (\x -> sumDigits x == n) [1..]
-- firstToSum 40 --> Just 49999


--
-- Mapping keys to values
--

phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> k == key) $ xs
-- findKey "penny" phoneBook --> "853-2492"
-- findKey "mark" phoneBook  --> "*** Exception: Prelude.head: empty list

-- findKey' returning a Maybe data type to avoid exception
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey' key xs
-- findKey' "penny" phoneBook --> Just "853-2492"
-- findKey' "mark" phoneBook  --> Nothing

-- findKey'' using a foldr rather than explicitly writing the recursion
findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key xs = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing xs
-- findKey'' "penny" phoneBook --> Just "853-2492"
-- findKey'' "mark" phoneBook  --> Nothing


--
-- Data.Map
--

phoneBook' :: Map.Map String String
phoneBook' = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

newBook = Map.insert "grace" "341-9021" phoneBook'
-- Map.lookup "grace" newBook --> Just "341-9021"
-- Map.size phoneBook'        --> 6
-- Map.size newBook           --> 7


phonesToDigits :: String -> [Int]
phonesToDigits = map digitToInt . filter isDigit
-- phonesToDigits "948-4435" --> [9,4,8,4,4,3,5]

intBook = Map.map phonesToDigits phoneBook'
-- Map.lookup "betty" intBook --> Just [5,5,5,2,9,3,8]


extendedPhoneBook =
    [("betty", "555-2938")
    ,("betty", "342-2492")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("patsy", "943-2929")
    ,("patsy", "827-9162")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ,("penny", "555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k,String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add phone1 phone2 = phone1 ++ ", " ++ phone2
-- Map.lookup "betty" $ phoneBookToMap extendedPhoneBook --> Just "342-2492, 555-2938"
-- Map.lookup "patsy" $ phoneBookToMap extendedPhoneBook --> Just "827-9162, 943-2929, 493-2928"
-- Map.lookup "wendy" $ phoneBookToMap extendedPhoneBook --> Just "939-8282"
-- Map.lookup "marky" $ phoneBookToMap extendedPhoneBook --> Nothing

-- lets first make all the values in the association list singleton lists
-- and then use ++ to combine the numbers
phoneBookToMap' :: (Ord k) => [(k,a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
-- Map.lookup "betty" $ phoneBookToMap' extendedPhoneBook --> Just ["342-2492","555-2938"]
-- Map.lookup "patsy" $ phoneBookToMap' extendedPhoneBook --> Just ["827-9162","943-2929","493-2928"]
-- Map.lookup "wendy" $ phoneBookToMap' extendedPhoneBook --> Just ["939-8282"]
-- Map.lookup "marky" $ phoneBookToMap' extendedPhoneBook --> Nothing

