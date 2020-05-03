module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Logic
import Coord
import Tetro
import Piece
import Draw


metrics :: Metrics
metrics = Metrics
  { screenWidth = 400
  , screenHeight = 800
  , blockSize = 32
  , boardOffset = (0, 0)
  , vanishZoneHeight = 0
  , gutter = 0.125
  }

main :: IO ()
main = play  
  (InWindow "Draw" (screenWidth metrics, screenHeight metrics) (0, 0))
  black 
  60 
  (initState 10 24 $ cycle [0..6])
  (draw metrics) 
  onEvent 
  step
        
onEvent :: Event -> State -> State
onEvent event state
  | EventKey (Char 'h') Down _ _ <- event
  , State b (Playing (Fall p) t) _ <- state
  , Just p' <- maybeMovePiece b West p
  = state { getMode = Playing (Fall p') t }

  | EventKey (Char 'l') Down _ _ <- event
  , State b (Playing (Fall p) t) _ <- state
  , Just p' <- maybeMovePiece b East p
  = state { getMode = Playing (Fall p') t }

  | EventKey (Char 'j') Down _ _ <- event
  , State b (Playing (Fall p) t) _ <- state
  , Just p' <- maybeRotatePiece b CCW p
  = state { getMode = Playing (Fall p') t }

  | EventKey (Char 'k') Down _ _ <- event
  , State b (Playing (Fall p) t) _ <- state
  , Just p' <- maybeRotatePiece b CW p
  = state { getMode = Playing (Fall p') t }

  | otherwise = state
