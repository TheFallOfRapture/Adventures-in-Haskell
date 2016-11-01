module Geometry.Cuboid
    ( volume
    , area
    ) where

volume :: Float -> Float -> Float -> Float
volume l w h = l * w * h

area :: Float -> Float -> Float -> Float
area l w h = (2 * l * w) + (2 * l * h) + (2 * w * h)
