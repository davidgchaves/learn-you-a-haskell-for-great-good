-- Making your own types and type classes

--
-- The custom Shape type
--

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- :t Circle    --> Circle :: Float -> Float -> Float -> Shape
-- :t Rectangle --> Rectangle :: Float -> Float -> Float -> Float -> Shape


area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- area $ Circle 10 20 10       --> 314.15927
-- area $ Rectangle 0 0 100 100 --> 10000.0

