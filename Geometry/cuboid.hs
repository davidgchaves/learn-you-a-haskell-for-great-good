module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = c * rectArea a b

area :: Float -> Float -> Float -> Float
area a b c = 2 * rectArea a b + 2 * rectArea a c + 2 * rectArea b c

rectArea :: Float -> Float -> Float
rectArea a b = a * b

