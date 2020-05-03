{-# LANGUAGE TupleSections #-}
module Draw where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Logic
import Coord
import Board
import Tetro
import Piece

data BlockGraphic = TetroBlock Tetro 
                  | WallBlock 
                  | ClearingBlock 
                  deriving (Show, Eq)

data Metrics = Metrics
  { screenWidth :: Int
  , screenHeight :: Int
  , blockSize :: Float
  , boardOffset :: Point
  , vanishZoneHeight :: Int
  , gutter :: Float
  }

draw :: Metrics -> State -> Picture
draw m s = translate (-(w/2)) (h/2) 
         . Pictures $ scale bs (-bs) 
         <$> [ drawBorder g v b
             , drawBoard g v b
             , piece
             ]
  where bs = blockSize m
        v  = vanishZoneHeight m
        g  = gutter m
        b  = getBoard s
        [w, h] = toEnum <$> [screenWidth m, screenHeight m]
        piece  = case getMode s of 
                 Playing (Fall p) _ -> drawPiece g v p
                 _                  -> Blank


drawBorder :: Float -> Int -> Board -> Picture
drawBorder _ _ (Board []) = Blank
drawBorder g v (Board xxs) = Pictures $ draw' <$> cs
  where
    xxs' = drop v xxs
    h = length xxs'
    w = length $ head xxs'
    draw' = drawBlock g WallBlock
    cs = concat [ (,) (-1) <$> [0..(h-1)] -- left
                , (,) w <$> [0..(h-1)]    -- right
                , (,h) <$> [(-1)..w]      -- bottom
                ]



drawBoard :: Float -> Int -> Board -> Picture
drawBoard g vanish = Pictures 
          . fmap drawRow 
          . zip ([0..] :: [Int])
          . drop vanish 
          . unBoard
  where drawRow :: (Int, [Cell]) -> Picture
        drawRow (y, row) = Pictures . fmap drawCell $ zip [0..] row
          where drawCell (x, cell) = case cell of
                                       Nothing -> Blank
                                       Just t  -> drawBlock g (TetroBlock t) (x, y)


drawClearingLines :: Board -> Float -> Int -> Int -> Picture
drawClearingLines b time y count = undefined
  where w = length . head $  unBoard b

drawPiece :: Float -> Int -> Piece -> Picture
drawPiece g vanish p@(Piece t _ _) = Pictures $ drawBlock g bg <$> cs
  where bg = TetroBlock t
        cs = filter (\(_, y) -> y > vanish-1) . unWorld $ getWorldCoords p

drawBlock :: Float -> BlockGraphic -> Coord -> Picture
drawBlock g bg c = translate x y $ blockGraphic bg
  where (x, y) = coord2Point g c


blockGraphic :: BlockGraphic -> Picture
blockGraphic (TetroBlock t) = Pictures 
      [ color col2 $ polygon p1 
      , color col1 $ polygon p2]
  where p1 = [ (0, 0), (1, 0), (0, 1) ]
        p2 = [ (1, 0), (1, 1), (0, 1) ]
        col1 = tetroColor t
        col2 = mixColors 1 0.25 col1 white

blockGraphic WallBlock = translate 0.5 0.5 
                       . Color (greyN 0.5) 
                       $ rectangleSolid 1 1

blockGraphic ClearingBlock = translate 0.5 0.5 
                           . Color white
                           $ rectangleSolid 1 1

  

coord2Point :: Float -> Coord -> Point
coord2Point g (x, y) = (fx + fx*g, fy + fy*g)
  where fx = toEnum x
        fy = toEnum y

tetroColor :: Tetro -> Color
tetroColor I = makeColorI 3 194 252 255
tetroColor O = makeColorI 255 222 8 255
tetroColor T = makeColorI 142 55 166 255
tetroColor S = makeColorI 44 150 67 255
tetroColor Z = makeColorI 181 58 58 255
tetroColor J = makeColorI 69 72 140 255
tetroColor L = makeColorI 255 157 0 255


