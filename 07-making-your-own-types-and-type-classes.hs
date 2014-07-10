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


nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) xshift yshift
    = Circle (Point (x + xshift) (y + yshift)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) xshift yshift
    = Rectangle (Point (x1 + xshift) (y1 + yshift)) (Point (x2 + xshift) (y2 + yshift))
-- nudge (Circle (Point 34 34) 10) 10 20               --> Circle (Point 44.0 54.0) 10.0
-- nudge (Rectangle (Point 10 10) (Point 20 20)) 15 25 --> Rectangle (Point 25.0 35.0) (Point 35.0 45.0)

