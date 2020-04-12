module Tetro where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Misc (Point)

data Tetro = O | I | S | Z | L | J | T deriving (Show, Eq)

data Rotation = R0 | R1 | R2 | R3 deriving (Show, Eq)

data RotDir = CW | CCW deriving (Show)


pickTetro :: Integral a => a -> Tetro
pickTetro  0 = O
pickTetro  1 = I
pickTetro  2 = S
pickTetro  3 = Z
pickTetro  4 = L
pickTetro  5 = J
pickTetro  6 = T
pickTetro  n = pickTetro $ n `rem` 7


rotate :: RotDir -> Rotation -> Rotation
rotate  CW R0 = R1
rotate  CW R1 = R2
rotate  CW R2 = R3
rotate  CW R3 = R0
rotate CCW R0 = R3
rotate CCW R1 = R0
rotate CCW R2 = R1
rotate CCW R3 = R2
                  

showPoints :: [Point] -> String
showPoints ps = unlines . transpose . chunksOf 4 $ plot <$> qs
  where qs = [(x, y) | x <- [0..3], y <- [0..3]]
        plot p = if p `elem` ps then '#' else '.'


-- Using 'Super Rotation System' (https://tetris.fandom.com/wiki/SRS)
points :: Tetro -> Rotation -> [Point]

points O _  = [ (1, 0), (2, 0), (1, 1), (2, 1) ]

points I R0 = [ (0, 1), (1, 1), (2, 1), (3, 1) ]
points I R1 = [ (2, 0), (2, 1), (2, 2), (2, 3) ]
points I R2 = [ (0, 2), (1, 2), (2, 2), (3, 2) ]
points I R3 = [ (1, 0), (1, 1), (1, 2), (1, 3) ]

points S R0 = [ (1, 0), (2, 0), (0, 1), (1, 1) ]
points S R1 = [ (1, 0), (1, 1), (2, 1), (2, 2) ]
points S R2 = [ (1, 1), (2, 1), (0, 2), (1, 2) ]
points S R3 = [ (0, 0), (0, 1), (1, 1), (1, 2) ]

points Z R0 = [ (0, 0), (1, 0), (1, 1), (2, 1) ]
points Z R1 = [ (2, 0), (1, 1), (2, 1), (1, 2) ]
points Z R2 = [ (0, 1), (1, 1), (1, 2), (2, 2) ]
points Z R3 = [ (1, 0), (0, 1), (1, 1), (0, 2) ]

points L R0 = [ (2, 0), (0, 1), (1, 1), (2, 1) ]
points L R1 = [ (1, 0), (1, 1), (1, 2), (2, 2) ]
points L R2 = [ (0, 1), (1, 1), (2, 1), (0, 2) ]
points L R3 = [ (0, 0), (1, 0), (1, 1), (1, 2) ]

points J R0 = [ (0, 0), (0, 1), (1, 1), (2, 1) ]
points J R1 = [ (1, 0), (2, 0), (1, 1), (1, 2) ]
points J R2 = [ (0, 1), (1, 1), (2, 1), (2, 2) ]
points J R3 = [ (1, 0), (1, 1), (0, 2), (1, 2) ]

points T R0 = [ (1, 0), (0, 1), (1, 1), (2, 1) ]
points T R1 = [ (1, 0), (1, 1), (2, 1), (1, 2) ]
points T R2 = [ (0, 1), (1, 1), (2, 1), (1, 2) ]
points T R3 = [ (1, 0), (0, 1), (1, 1), (1, 2) ]

