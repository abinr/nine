module ExNineSpec (main, spec) where

import Test.Hspec
import Nine (pack)

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "Pack consecutive duplicates into sublists" $ do
    it "should return an empty list for an empty list" $ do
      pack [] `shouldBe` ([] :: [[Int]])
    it "should return a single element list for the same" $ do
      pack [1] `shouldBe` ([[1]])
    it "should return sublists for unique elements" $ do
      pack [1, 2] `shouldBe` [[1], [2]]
    it "should pack consecutive dupls into sublists" $ do
      pack [1,1,2] `shouldBe` [[1,1], [2]]
    it "should be polymorphic" $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
       `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

