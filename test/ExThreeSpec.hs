module ExThreeSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Find the Kth element of a list. First element is number 1" $ do
    it "should find the Kth element as a Maybe" $ do
      elementAt [1,2,3] 2 `shouldBe` Just 2
    it "should return Nothing for empty list" $ do
      elementAt ([]::[Char]) 2 `shouldBe` Nothing
    it "should return Nothing when k < 1" $ do
      elementAt [1] 0 `shouldBe` Nothing
    it "should return Nothing when k < length list" $ do
      elementAt [1] 2 `shouldBe` Nothing
    it "should be polymorphic" $ do
      elementAt ['a'..'z'] 25 `shouldBe` Just 'y'

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:_) 1 = Just x
elementAt (_:xs) k
  | k < 1 = Nothing
  | otherwise = elementAt xs (k - 1)
