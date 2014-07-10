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


baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r
--  nudge (baseCircle 10) 30 40 -->  Circle (Point 30.0 40.0) 10.0


baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)
-- nudge (baseRectangle 40 100) 60 23 --> Rectangle (Point 60.0 23.0) (Point 100.0 123.0)


--
-- The custom Person type using Record Syntax
--

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)
-- :t flavor    --> flavor :: Person -> String
-- :t firstName --> firstName :: Person -> String
-- Person {firstName = "Buddy", lastName = "Finklestein", phoneNumber = "444-123456", flavor = "Chocolate", age = 43, height = 1.84 }
--  --> Person {firstName = "Buddy", lastName = "Finklestein", age = 43, height = 1.84, phoneNumber = "444-123456", flavor = "Chocolate"}

