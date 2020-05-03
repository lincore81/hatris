 module Logic where

import Piece
import Board
import Coord
import Timer

data GameEvent = Fall Piece 
               | Spawn 
               | ClearLines
               deriving (Show)

data Mode = Playing GameEvent Timer
          | GameOver 
          deriving (Show)

data State = State 
    { getBoard :: Board
    , getMode :: Mode
    , getRandoms :: [Int]
    } 

instance Show State where
    show s = unlines [ show (getBoard s)
                     , show (take 10 $ getRandoms s)
                     , show (getMode s)
                     ]

spawnCoord :: Coord
spawnCoord = (3, 0)

initState :: Int -> Int -> [Int] -> State
initState w h xs = State 
  { getBoard   = mkBoard w h
  , getMode    = Playing Spawn $ mkTimer 0
  , getRandoms = if xs /= [] then xs 
                  else cycle [6, 2, 0, 5, 1, 3, 1, 5, 3, 4, 0, 5, 6, 2, 6, 2, 1, 5, 3, 0, 4, 3, 0, 6, 4, 2, 4, 1, 6, 2]
  }

-- data PlayerAction = TurnLeft | TurnRight | MoveLeft | MoveRight

-- handleInput :: PlayerAction -> State -> Either () (Maybe State)
-- handleInput action s 
--   | isFalling s = case action of
--               TurnLeft  -> Right $ turn CCW s
--               TurnRight -> Right $ turn CW  s
--               MoveLeft  -> Right $ move West s
--               MoveRight -> Right $ move East s
--   | otherwise = Left ()

isFalling :: State -> Bool
isFalling s = case getMode s of
                Playing (Fall _) _ -> True
                _                  -> False

isGameOver :: State -> Bool
isGameOver s = case getMode s of
                GameOver -> True
                _        -> False

-- advance simulation by delta seconds
step :: Float -> State -> State 
step δ s = case getMode s of
             GameOver                   -> s                
             Playing e t | isElapsed t' -> handleEvent e s
                         | otherwise    -> s { getMode = Playing e t' }
               where t' = updateTimer δ t

fallSpeed :: Float
fallSpeed = 0.2

clearDelay :: Float
clearDelay = 0.5

spawnDelay :: Float
spawnDelay = 0.5

fall :: Piece -> State -> State
fall p s = s { getMode = Playing (Fall p) (mkTimer fallSpeed) }

settle :: Piece -> State -> State
settle p s = s { getMode = mode, getBoard = b' }
  where 
    b  = settlePiece (getBoard s) p
    xs = findFullRows b
    (b', mode) = case xs of
             [] -> (b, Playing Spawn (mkTimer spawnDelay))
             _  -> (clearRows xs b, Playing ClearLines (mkTimer clearDelay))



handleEvent :: GameEvent -> State -> State
handleEvent (Fall p) s = case maybeMovePiece b South p of 
    Just p' -> fall p' s
    Nothing -> settle p s
    where 
      b = getBoard s

handleEvent Spawn s = if canFitPiece b p 
                      then s' else gameOver s
  where (r:rs) = getRandoms s
        b = getBoard s
        p = mkPiece spawnCoord r
        m = Playing (Fall p) (mkTimer fallSpeed)
        s' = s {getMode = m, getRandoms = rs}

handleEvent ClearLines s = s { 
  getMode = Playing Spawn (mkTimer spawnDelay),
  getBoard = collapse b }
    where b = getBoard s

gameOver :: State -> State
gameOver s = s {getMode = GameOver}



simulate :: Int -> State -> [State]
simulate n s = simulate' [s] n s

simulate' :: [State] -> Int -> State -> [State]
simulate' acc steps s | steps <= 0 = acc
                      | otherwise  = simulate' (s':acc) (steps-1) s'
                     where s' = step 1 s
