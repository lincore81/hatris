module Board where

import Data.Maybe (isNothing, isJust, maybe)
import Data.List (replicate, partition)
import Tetro (Tetro, Rotation)
import Coord (WorldCoords (WorldCoords), Coord, addCoords)
import Misc

type Cell = Maybe Tetro
newtype Board = Board { unBoard :: [[Cell]] } deriving (Eq)

instance Show Board where
  show = unlines . map (map f) . unBoard
    where f Nothing  = '.'
          f (Just x) = head $ show x

mkBoard :: Int -> Int -> Board
mkBoard w h = Board . replicate h $ replicate w Nothing

getCell :: Board -> Coord -> Maybe Cell
getCell b (x, y) = nth y (unBoard b) >>= nth x

allEmpty :: Board -> WorldCoords -> Bool
allEmpty b (WorldCoords xs) = all empty $ getCell b <$> xs
  where empty (Just Nothing) = True
        empty _ = False


-- Put a tetro on the board at the given coords.
-- Coords not in bounds are ignored.
putCells :: Board -> WorldCoords -> Tetro -> Board
putCells b (WorldCoords cs) t = Board . indexedMap updateRow $ unBoard b
  where updateRow (y, rs) = indexedMap updateCell rs
            where updateCell (x, m) = if (x, y) `elem` cs
                                      then Just t
                                      else m

findFullRows :: Board -> [Int]
findFullRows = fmap fst 
             . filter (all isJust . snd) 
             . zip [0..]
             . unBoard

clearRows :: [Int] -> Board -> Board
clearRows ys = Board . fmap clear . zip [0..] . unBoard
  where clear (y, row) = if y `elem` ys
                         then Nothing <$ row
                         else row

-- | Remove all empty rows and put them on 'top' of the board 
collapse :: Board -> Board
collapse = Board 
         . uncurry mappend 
         . partition (all isNothing) 
         . unBoard



