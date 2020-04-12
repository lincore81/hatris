module Misc where

type Point = (Int, Int)
data Direction = West | East | South

fromDirection :: Direction -> Point
fromDirection West = (-1, 0)
fromDirection East = (1, 0)
fromDirection South = (0, 1)

addPoints :: Point -> Point -> Point
addPoints (x0, y0) (x1, y1) = (x0+x1, y0+y1)


