module Level where

type Gravity = Float
type Lines = Int
type Score = Int

newtype LineClear = LineClear { unLineClear :: Int }
newtype LineClearError = IllegalLineCountError Int
mkLineClear :: Int -> Either LineClearError LineClear
mkLineClear n | n `elem` [1..4] = Right $ LineClear n
              | otherwise       = Left  $ IllegalLineCountError n

newtype Level = Level { unLevel :: Int }
data LevelError = NegativeLevelError
mkLevel :: Int -> Either LevelError Level
mkLevel n | n >= 0    = Right $ Level n
          | otherwise = Left NegativeLevelError

-- the number of frames (at 60fps) it takes to drop a tetromino by one unit.

level :: Lines -> Either LevelError Level
level = mkLevel . (`div` 10)

score :: Score -> LineClear -> Level -> Score
score s c l = (+s) $ l' * c'
  where l' = unLevel l
        c' = unLineClear c

-- Using NES Tetris frame counts
gravity :: Level -> Gravity
gravity = (/60) . (fs !!) . unLevel
  where fs = [ 48, 43, 38, 33, 28             -- lvl  0 -  4
             , 23, 18, 13,  8,  6             -- lvl  5 -  9
             , 5, 5, 5,  4, 4, 4,  3, 3, 3    -- lvl 10 - 18
             , 2, 2, 2,  2, 2, 2,  2, 2, 2, 2 -- lvl 19 - 28
             ] ++ repeat 1                    -- lvl 29+
