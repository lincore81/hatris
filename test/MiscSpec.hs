module MiscSpec where
import Test.Hspec
import Misc

spec :: Spec
spec = describe "nth" $ do
  it "should return Nothing if n < 0" $
    nth (-1) xs `shouldBe` Nothing    
  it "should return Nothing if n >= length xs" $
    nth 100 xs `shouldBe` Nothing    
  it "should return xs !! n if 0 <= n < length" $
    nth 50 xs `shouldBe` Just 50
  where xs = [0..99] :: [Integer]


