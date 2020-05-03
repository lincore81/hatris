module BoardSpec where

import Test.Hspec
import Board
import Tetro
import Coord

spec :: Spec
spec = do
  describe "getCell" $ do
    it "yield Nothing when the coords are out of bounds (x<0)" $
      getCell b (-1, 0) `shouldBe` Nothing
    it "yield Nothing when the coords are out of bounds (x>W)" $
      getCell b (3, 0) `shouldBe` Nothing
    it "yield Nothing when the coords are out of bounds (y<0)" $
      getCell b (0, -1) `shouldBe` Nothing
    it "yield Nothing when the coords are out of bounds (y>W)" $
      getCell b (0, 3) `shouldBe` Nothing
    it "yield Just Nothing when the cell is empty" $
      getCell b (0, 0) `shouldBe` Just Nothing
    it "yield Just t when the cell contains t" $ do
      getCell b (0, 1) `shouldBe` (Just $ Just T)
      getCell b (1, 0) `shouldBe` (Just $ Just O)

  describe "allEmpty" $ do
    it "yield False for an empty board and 1 coord" $
      allEmpty (Board []) (WorldCoords [(0, 0)]) `shouldBe` False
    it "yield True if all cells are empty and in bounds" $
      allEmpty b (WorldCoords [(0, 0), (2, 0), (0, 2), (2, 2)]) `shouldBe` True
    it "yield True if no coords are given" $
      allEmpty (Board []) (WorldCoords []) `shouldBe` True
    it "yield False if any coord's cell is occupied" $
      allEmpty b (WorldCoords [(0, 0), (2, 0), (0, 1)]) `shouldBe` False
    it "yield False if any coord's cell is out of bounds" $
      allEmpty b (WorldCoords [(0, 0), (2, 0), (0, -1)]) `shouldBe` False

  describe "putCells" $ do
    it "a previously set cell with putCells read with getCell will have the same value" $ do
      let b' = putCells (Board [[Nothing]]) (WorldCoords [(0, 0)]) O 
      getCell b' (0, 0) `shouldBe` (Just $ Just O)
      let b'' = putCells b (WorldCoords [(2, 2)]) I
      getCell b'' (2, 2) `shouldBe` (Just $ Just I)
    it "trying to put cells out of bounds will not change the board" $
      putCells b (WorldCoords [(-1, 0), (0, 4), (3, -9)]) I `shouldBe` b
  
  describe "findFullRows" $ do
    it "yields an empty list when given an empty board" $
      findFullRows (Board []) `shouldBe` []
    it "yields an empty list when the board has no filled rows" $
      findFullRows b2 `shouldBe` []
    it "yields the correct index of every filled row" $ do
      findFullRows b3 `shouldBe` [0, 2]
      findFullRows b `shouldBe` [1]

  describe "clearRows" $ do
    it "yields an unchanged board when not given any row indezes" $
      clearRows [] b `shouldBe` b
    it "yields an unchanged board when all indezes are out of bounds" $
      clearRows [-3, -1, 1221] b `shouldBe` b
    it "fills all indexed rows with Nothing" $ do
      clearRows [1] b `shouldBe` bc
      clearRows [0, 2] b3 `shouldBe` bc3

    where 
      b = Board [ [ Nothing, Just O, Nothing]
                , [ Just T,  Just T, Just T ]
                , [ Nothing, Just T, Nothing]
                ]
      bc = Board [ [ Nothing, Just O, Nothing]
                 , [ Nothing, Nothing, Nothing]
                 , [ Nothing, Just T, Nothing]
                 ]

      b2 = Board [ [ Nothing, Just O, Nothing]
                 , [ Nothing, Just T, Just T ]
                 , [ Nothing, Just T, Nothing]
                 ]
      b3 = Board [ [ Just O , Just O, Just O ]
                 , [ Nothing, Just T, Just T ]
                 , [ Just T , Just T, Just T ]
                 ]
      bc3 = Board [ [ Nothing , Nothing, Nothing ]
                  , [ Nothing, Just T, Just T ]
                  , [ Nothing , Nothing, Nothing ]
                  ]
