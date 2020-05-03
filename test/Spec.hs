{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main where
import Test.Hspec
import Point


main :: IO ()
main = hspec $ 
  describe "addPoints" $ 
    it "Returns the sum of the given points" $
      addPoints (3, 6) (4, 7) `shouldBe` (7, 13)
