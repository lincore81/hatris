module Coord where

import Data.List (transpose)
import Data.List.Split (chunksOf)

type Coord = (Int, Int)
newtype LocalCoords = LocalCoords { unLocal :: [Coord] } deriving (Show)
newtype WorldCoords = WorldCoords { unWorld :: [Coord] } deriving (Show)

toWorld :: Coord -> LocalCoords -> WorldCoords
toWorld o (LocalCoords xs) = WorldCoords $ offset xs
  where offset = fmap (addCoords o)


addCoords :: Coord -> Coord -> Coord
addCoords (x0, y0) (x1, y1) = (x0+x1, y0+y1)

showCoords :: [Coord] -> String
showCoords ps = unlines . transpose . chunksOf 4 $ plot <$> qs
  where qs = [(x, y) | x <- [0..3], y <- [0..3]]
        plot p = if p `elem` ps then '#' else '.'


data Cardinal = West | East | North | South

fromCardinal :: Cardinal -> Coord
fromCardinal West  = (-1,  0)
fromCardinal East  = ( 1,  0)
fromCardinal North = ( 0, -1)
fromCardinal South = ( 0,  1)

addCardinal :: Coord -> Cardinal -> Coord
addCardinal p = addCoords p . fromCardinal
