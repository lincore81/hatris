module Main where
import Test.Hspec
import Misc (addPoints)


main :: IO ()
main = hspec $ 
  describe "addPoints" $ 
    it "Returns the sum of the given points" $
      addPoints (3, 6) (4, 7) `shouldBe` (7, 13)
