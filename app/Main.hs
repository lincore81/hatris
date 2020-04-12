{-# LANGUAGE PatternGuards #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (maybe)

main :: IO ()
main = do 
  let state = State Nothing []
  play  (InWindow "Draw" (600, 600) (0, 0))
        white 100 state
        makePicture handleEvent stepWorld

data State = State (Maybe Path) [Picture]

type Segment = ((Float, Float), (Float, Float))

makePicture :: State -> Picture
makePicture (State m xs) = Pictures (maybe xs (\x -> Line x : xs) m)

handleEvent :: Event -> State -> State
handleEvent event state
  | EventMotion (x, y) <- event
  , State (Just ps) ss  <- state
  = State (Just ((x, y):ps)) ss

  | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
  , State Nothing ss <- state
  = State (Just [pt]) 
          ((Translate x y $ Scale 0.1 0.1 $ Text "Down") : ss)

  | EventKey (MouseButton LeftButton) Up _ pt@(x,y) <- event
  , State (Just ps) ss <- state
  = State Nothing
          ((Translate x y $ Scale 0.1 0.1 $ Text "Up") : Line (pt:ps) : ss)

  | otherwise = state

stepWorld :: Float -> State -> State
stepWorld _ = id
