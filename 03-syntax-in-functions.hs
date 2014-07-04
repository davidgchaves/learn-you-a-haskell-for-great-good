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

