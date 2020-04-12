module Action where

import State
import Tetro
import Misc
import Board

type Index = Int

data MoveDir = MoveLeft | MoveRight
  deriving (Show, Eq)

data PlayerAction
  = Rotate RotDir
  | Move MoveDir

data GameAction
  = Spawn State Float
  | Settle State

spawnPiece :: State -> Pos -> Index -> State
spawnPiece    s        p      i      = s
  { getTetro = Just t
  , getPos = p
  , getRot = R0
  , alive = tryTetro b p t R0
  }
  where t = pickTetro i
        b = getBoard s


settle :: State -> State

drop1 :: State -> State
drop1 s = case getTetro s of
  Nothing -> s
  Just t | cantFit -> settle s
         | otherwise -> s {getPos = p}


updateState :: State -> PlayerAction -> State
updateState s (Rotate d) = case getTetro s of
  Nothing -> s
  Just t  | cantFit   -> s
          | otherwise -> s {getRot = r'}
    where b = getBoard s
          p = getPos s
          r = getRot s
          r' = rotate d r
          cantFit = not $ tryTetro b p t r'

updateState s (Move d) = case getTetro s of
  Nothing -> s
  Just t | cantFit -> s
         | otherwise -> s {getPos = p}
    where b = getBoard s
          (x, y) = getPos s
          m = if d == MoveLeft then -1 else 1
          p = (x+m, y)
          r = getRot s
          cantFit = not $ tryTetro b p t r
