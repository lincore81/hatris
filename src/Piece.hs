module Piece where

import Data.Maybe (isNothing)
import Coord 
  ( addCardinal
  , addCoords
  , toWorld
  , Cardinal (..)
  , Coord
  , LocalCoords
  , WorldCoords
  )
import Board (Board, allEmpty, putCells)
import Tetro 
  ( pickTetro
  , Rotation(R0)
  , RotDir(..)
  , Tetro
  , coords
  , rotate
  )
import Misc

data Piece = Piece Tetro Rotation Coord deriving (Show)

mkPiece :: Coord -> Int -> Piece
mkPiece cs n = Piece (pickTetro n) R0 cs

getLocalCoords :: Piece -> LocalCoords
getLocalCoords (Piece t r _) = coords t r

getWorldCoords :: Piece -> WorldCoords
getWorldCoords (Piece t r o) = toWorld o $ coords t r

maybeMovePiece :: Board -> Cardinal -> Piece -> Maybe Piece
maybeMovePiece b c (Piece t r o) 
  = if   allEmpty b $ getWorldCoords p'
    then Just p'
    else Nothing
    where 
      o' = addCardinal o c
      p' = Piece t r o'

-- TODO: handle wallkick, tspin etc.
maybeRotatePiece :: Board -> RotDir -> Piece -> Maybe Piece
maybeRotatePiece b d p@(Piece t r o)
  = justIf (Piece t r' o) 
  . allEmpty b 
  $ getWorldCoords p
  where r' = rotate d r

canFitPiece :: Board -> Piece -> Bool
canFitPiece b p = allEmpty b $ getWorldCoords p

settlePiece :: Board -> Piece -> Board
settlePiece b (Piece t r o) = putCells b cs t
  where cs = toWorld o $ coords t r

