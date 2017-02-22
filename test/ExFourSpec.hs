module ExFourSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Find the number of elements in a list" $ do
    it "Return 3 for a 3 element list" $ do
      myLength [123, 456, 789] `shouldBe` 3
    it "Return zero for empty list" $ do
      myLength [] `shouldBe` 0
    it "Should be polymorphic" $ do
      myLength ['a'..'z'] `shouldBe` 26
    it "Should calculate length of an arbitrary list" $
      property $ \xs -> myLength xs == length (xs :: [Int])

myLength :: [a] -> Int
myLength list =
  let
    aux list n =
      case list of
        [] ->
          n
        x:xs ->
          aux xs (n + 1)
  in
    aux list 0
