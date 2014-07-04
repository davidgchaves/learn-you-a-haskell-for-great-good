-- Believe the Type

--
-- Explicit Type Declaration
--

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


--
-- Common Haskell Types
--

boundedFactorial :: Int -> Int
boundedFactorial n = product [1..n]

notBoundedFactorial :: Integer -> Integer
notBoundedFactorial n = product [1..n]

singlePrecisionCircumference :: Float -> Float
singlePrecisionCircumference r = 2 * pi * r

doublePrecisionCircumference :: Double -> Double
doublePrecisionCircumference r = 2 * pi * r

fromCharToTrue :: Char -> Bool
fromCharToTrue c = True

