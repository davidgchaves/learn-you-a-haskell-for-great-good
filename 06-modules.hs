-- Modules

--
-- Importing modules
--

import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- numUniques [1,2,3,4,5,1,2,3,5] --> 5

