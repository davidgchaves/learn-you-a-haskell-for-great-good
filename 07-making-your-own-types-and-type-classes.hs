-- Making your own types and type classes

--
-- The custom Shape type
--

data Point = Point Float Float
    deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)
-- :t Circle    --> Circle :: Point -> Float -> Shape
-- :t Rectangle --> Rectangle :: Point -> Point -> Shape


area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- area $ Circle (Point 0 0) 10                 --> 314.15927
-- area $ Rectangle (Point 0 0) (Point 100 100) --> 10000.0

