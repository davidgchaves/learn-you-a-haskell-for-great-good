-- Modules

--
-- Importing modules
--

import Data.List

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

