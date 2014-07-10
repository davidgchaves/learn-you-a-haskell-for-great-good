-- Making your own types and type classes

--
-- The custom Shape type
--

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- :t Circle    --> Circle :: Float -> Float -> Float -> Shape
-- :t Rectangle --> Rectangle :: Float -> Float -> Float -> Float -> Shape

