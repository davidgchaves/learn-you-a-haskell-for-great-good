-- to use the Geometry module, just:
-- import Geometry
-- Geometry.hs must be in the same folder as the module that's importing it

module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidVolume
, cuboidArea
) where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * (r ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = c * rectArea a b

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = 2 * rectArea a b + 2 * rectArea a c + 2 * rectArea b c

rectArea :: Float -> Float -> Float
rectArea a b = a * b

