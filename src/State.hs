module State where

{-
Tetris state update:

handle input:
  - rotate piece
  - move piece
  - no action

add delta to fall time
when time elpased, drop piece by 1 unit:
  if new pos does not fit, settle
    if lines have been filled:
       add score
       clear lines
       wait
       if level has increased, play level+ animation
    spawn new piece
    if piece can not be spawned, game over
-}

import Tetro
import Board
import Misc (Point)
import Action

type Level = Int
type Lines = Int
type Score = Int

-- the number of frames (at 60fps) it takes to drop a tetromino by one unit.
type Gravity = Float


level :: Lines -> Level
level = (`div` 10)

score :: Lines -> Level -> Either String Score
score 1 l = Right $   40 * (l+1)
score 2 l = Right $  100 * (l+1)
score 3 l = Right $  300 * (l+1)
score 4 l = Right $ 1200 * (l+1)
score n _ = Left  $ "Illegal line count: " ++ show n

-- Using NES Tetris frame counts
gravity :: Level -> Either String Gravity
gravity n
  | isNegative n = Left  $ "Given level must not be negative: " ++ show n
  | otherwise    = Right f
  where 
    isNegative = (==-1) . signum
    f  = (/60) $ fs !! n
    fs = [ 48, 43, 38, 33, 28             -- lvl  0 -  4
         , 23, 18, 13,  8,  6             -- lvl  5 -  9
         , 5, 5, 5,  4, 4, 4,  3, 3, 3    -- lvl 10 - 18
         , 2, 2, 2,  2, 2, 2,  2, 2, 2, 2 -- lvl 19 - 28
         ] ++ repeat 1                    -- lvl 29+

data State = State 
    { getTetro :: Maybe Tetro
    , getPos :: Point
    , getRot :: Rotation
    , getScore :: Score
    , getGravity :: Gravity
    , nextDrop :: Gravity
    , getBoard :: Board
    , alive :: Bool
    , waiting :: Maybe (Float, Action)
    } deriving (Show)

spawn :: Point
spawn = (3, 0)
boardSize :: Point
boardSize = (10, 24)


