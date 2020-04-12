module Board where

import Data.Maybe (isNothing)
import Data.List (replicate)
import Tetro (Tetro, Rotation, points)
import Misc (Point, Direction, fromDirection, addPoints)


type Cell = Maybe Tetro
type Board = [[Cell]]

mkBoard :: Int -> Int -> Board
mkBoard w h = replicate h $ replicate w Nothing

showBoard :: Board -> String
showBoard = unlines . map (map f) 
  where f Nothing  = '.'
        f (Just x) = head $ show x

getCell :: Board -> Point -> Cell
getCell bs (x, y) = bs !! y !! x

inBounds :: Board -> Point -> Bool
inBounds bs (x, y) = and [ y >= 0
                         , x >= 0
                         , y < length bs
                         , (x <) . length $ head bs
                         ]

offsetPiece :: Point -> [Point] -> [Point]
offsetPiece (px, py) = fmap (\(x, y) -> (px+x, py+y))

canFitPiece :: Board -> [Point] -> Bool
canFitPiece bs = all (\p -> inBounds bs p && isNothing (getCell bs p))
 
tryTetro :: Board -> Point -> Tetro -> Rotation -> Bool
tryTetro b pos t r = canFitPiece b . offsetPiece pos $ points t r

-- Put a tetromino on the board if it is in bounds
-- assumes the given points have been translated
settlePiece :: Board -> [Point] -> Tetro -> Board
settlePiece bs ps t = updateRow <$> zip [0..] bs
    where updateRow (y, rs) = updateCell <$> zip [0..] rs
            where updateCell (x, m) = if (x, y) `elem` ps
                                      then Just t
                                      else m

movePiece :: Board -> Point -> [Point] -> Direction -> Maybe Point
movePiece bs p ps d = 
  if canFitPiece bs ps
  then Just p'
  else Nothing
  where p' = addPoints p $ fromDirection d                         

clearRow :: Board -> Int -> Board
clearRow bs n = f <$> zip [0..] bs
  where f (y, r) = if y == n 
                   then Nothing <$ r 
                   else r


