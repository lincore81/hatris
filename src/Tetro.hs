module Tetro where

import Coord (LocalCoords (..))

data Tetro    = O | I | S | Z | L | J | T   deriving (Show, Eq, Enum)
data Rotation = R0 | R1 | R2 | R3           deriving (Show, Eq)
data RotDir   = CW | CCW                    deriving (Show, Eq)

pickTetro :: Int -> Tetro
pickTetro n = (!! n) $ cycle [O, I, S, Z, L, J, T]

rotate :: RotDir -> Rotation -> Rotation
rotate  CW R0 = R1
rotate  CW R1 = R2
rotate  CW R2 = R3
rotate  CW R3 = R0
rotate CCW R0 = R3
rotate CCW R1 = R0
rotate CCW R2 = R1
rotate CCW R3 = R2

-- Using 'Super Rotation System' (https://tetris.fandom.com/wiki/SRS)
coords :: Tetro -> Rotation -> LocalCoords

coords O _  = LocalCoords [ (1, 0), (2, 0), (1, 1), (2, 1) ]

coords I R0 = LocalCoords [ (0, 1), (1, 1), (2, 1), (3, 1) ]
coords I R1 = LocalCoords [ (2, 0), (2, 1), (2, 2), (2, 3) ]
coords I R2 = LocalCoords [ (0, 2), (1, 2), (2, 2), (3, 2) ]
coords I R3 = LocalCoords [ (1, 0), (1, 1), (1, 2), (1, 3) ]

coords S R0 = LocalCoords [ (1, 0), (2, 0), (0, 1), (1, 1) ]
coords S R1 = LocalCoords [ (1, 0), (1, 1), (2, 1), (2, 2) ]
coords S R2 = LocalCoords [ (1, 1), (2, 1), (0, 2), (1, 2) ]
coords S R3 = LocalCoords [ (0, 0), (0, 1), (1, 1), (1, 2) ]

coords Z R0 = LocalCoords [ (0, 0), (1, 0), (1, 1), (2, 1) ]
coords Z R1 = LocalCoords [ (2, 0), (1, 1), (2, 1), (1, 2) ]
coords Z R2 = LocalCoords [ (0, 1), (1, 1), (1, 2), (2, 2) ]
coords Z R3 = LocalCoords [ (1, 0), (0, 1), (1, 1), (0, 2) ]

coords L R0 = LocalCoords [ (2, 0), (0, 1), (1, 1), (2, 1) ]
coords L R1 = LocalCoords [ (1, 0), (1, 1), (1, 2), (2, 2) ]
coords L R2 = LocalCoords [ (0, 1), (1, 1), (2, 1), (0, 2) ]
coords L R3 = LocalCoords [ (0, 0), (1, 0), (1, 1), (1, 2) ]

coords J R0 = LocalCoords [ (0, 0), (0, 1), (1, 1), (2, 1) ]
coords J R1 = LocalCoords [ (1, 0), (2, 0), (1, 1), (1, 2) ]
coords J R2 = LocalCoords [ (0, 1), (1, 1), (2, 1), (2, 2) ]
coords J R3 = LocalCoords [ (1, 0), (1, 1), (0, 2), (1, 2) ]

coords T R0 = LocalCoords [ (1, 0), (0, 1), (1, 1), (2, 1) ]
coords T R1 = LocalCoords [ (1, 0), (1, 1), (2, 1), (1, 2) ]
coords T R2 = LocalCoords [ (0, 1), (1, 1), (2, 1), (1, 2) ]
coords T R3 = LocalCoords [ (1, 0), (0, 1), (1, 1), (1, 2) ]

