-- Modules

--
-- Importing modules
--

--
-- import zone
--
import Data.List
import Data.Char
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

